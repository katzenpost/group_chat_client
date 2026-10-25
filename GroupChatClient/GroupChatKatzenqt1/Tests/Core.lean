/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import GroupChatClient.GroupChatKatzenqt1.Tests.Net
import GroupChatClient.Tests.Util

/-! # The core

Members chatting over a simulated Pigeonhole, in one process, with no daemon. -/

namespace GroupChatClient.GroupChatKatzenqt1.Tests.CoreTests

open GroupChatClient GroupChatClient.Tests
open Cryptlib.Protocols.GroupComm.Common
open Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member
open GroupChatClient.GroupChatKatzenqt1.Persist

def base64 : TestM Unit := do
  for (raw, text) in [("", ""), ("a", "YQ=="), ("ab", "YWI="), ("abc", "YWJj"),
      ("hello world", "aGVsbG8gd29ybGQ=")] do
    check s!"base64 encodes {raw}" (Base64.encode raw.toUTF8 == text)
    check s!"base64 decodes {text}" ((Base64.decode text).map (·.data) == some raw.toUTF8.data)
  let bytes : ByteArray := ⟨(List.range 256).toArray.map (·.toUInt8)⟩
  check "base64 round trip" ((Base64.decode (Base64.encode bytes)).map (·.data) == some bytes.data)
  check "base64 refuses junk" (Base64.decode "abc!" |>.isNone)

def newNet (n : Nat) (names : List String) : IO (IO.Ref Sim.World × Net) := do
  let world ← IO.mkRef ({} : Sim.World)
  pure (world, { cores := (names.take n).toArray.map fun me => { present := .plain, state := { me } } })

/-- Alice founds "g", Bob joins with a voucher Alice inducts. -/
def foundAndJoin (world : IO.Ref Sim.World) : NetM Bool := do
  feed 0 (.create "g" none)
  feed 1 (.join "g" none)
  unless ← runUntil world fun n => (code? n 1 "g").isSome do return false
  let some code := code? (← get) 1 "g" | return false
  feed 0 (.induct "g" code)
  runUntil world fun n => joined n 1 "g" && noticed n 0 "g" "added ~bob"

def twoMembers : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let (ok, net) ← (do
    unless ← foundAndJoin world do return (false, false, false, false)
    feed 1 (.send "g" "hi from bob")
    feed 0 (.send "g" "hi from alice")
    let both ← runUntil world fun n => says n 0 "g" "hi from bob" && says n 1 "g" "hi from alice"
    -- Bob acknowledges what he read in his next message.
    feed 1 (.send "g" "read you")
    let acked ← runUntil world fun n => (chatOf n 0 "g").any fun (c : Chat) =>
      c.log.any fun l => l.text == "hi from alice" && acknowledged c l
    let n ← get
    let idle := n.cores.all fun c => c.state.chats.all (·.jobs.isEmpty)
    return (true, both, acked, idle)).run net
  let (joinedOk, both, acked, idle) := ok
  check "bob joins alice's group" joinedOk
  check "alice and bob read each other" both
  check "bob's message acknowledges alice's" acked
  check "nothing is left to write" idle
  let names := (net.cores[0]!.state.chat? "g").map fun (c : Chat) => c.names.values
  check "alice knows bob by name" (names.any fun (ns : List String) => ns.contains "bob")

/-- Each names themselves in the group as they found or join it, and goes by that there. -/
def ownNames : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let (ok, net) ← (do
    feed 0 (.create "g" (some "Ali"))
    feed 1 (.join "g" (some "Bo"))
    unless ← runUntil world fun n => (code? n 1 "g").isSome do return false
    let some code := code? (← get) 1 "g" | return false
    feed 0 (.induct "g" code)
    runUntil world fun n => joined n 1 "g" && noticed n 0 "g" "added ~Bo").run net
  check "bob joins as the name he gave" ok
  let names (i : Nat) := ((net.cores[i]!.state.chat? "g").map fun (c : Chat) => c.names.values).getD []
  check "alice knows him by it" ((names 0).contains "Bo" && !(names 0).contains "bob") fun _ => toString (names 0)
  check "and bob knows alice by the name she gave" ((names 1).contains "Ali" && !(names 1).contains "alice")
    fun _ => toString (names 1)
  check "each group keeps its member's name" ((net.cores[1]!.state.chat? "g").any (·.me == some "Bo"))

def threeMembers : TestM Unit := do
  let (world, net) ← newNet 3 ["alice", "bob", "carol"]
  let (ok, _) ← (do
    unless ← foundAndJoin world do return (false, false)
    -- Carol joins through Bob; Alice learns of her from Bob's Introduction.
    feed 2 (.join "g" none)
    unless ← runUntil world fun n => (code? n 2 "g").isSome do return (false, false)
    let some code := code? (← get) 2 "g" | return (false, false)
    feed 1 (.induct "g" code)
    unless ← runUntil world fun n => joined n 2 "g" && noticed n 0 "g" "~bob added ~carol" do
      return (false, false)
    feed 2 (.send "g" "carol here")
    feed 0 (.send "g" "welcome carol")
    let all ← runUntil world fun n =>
      says n 0 "g" "carol here" && says n 1 "g" "carol here" && says n 2 "g" "welcome carol"
    return (true, all)).run net
  check "carol joins through bob, and alice hears of it" ok.1
  check "all three read each other" ok.2

def removeAndLeave : TestM Unit := do
  let (world, net) ← newNet 3 ["alice", "bob", "carol"]
  let (ok, _) ← (do
    unless ← foundAndJoin world do return (false, false, false, false, false)
    feed 2 (.join "g" none)
    unless ← runUntil world fun n => (code? n 2 "g").isSome do return (false, false, false, false, false)
    let some code := code? (← get) 2 "g" | return (false, false, false, false, false)
    feed 0 (.induct "g" code)
    unless ← runUntil world fun n => joined n 2 "g" && noticed n 1 "g" "~alice added ~carol" do
      return (false, false, false, false, false)
    feed 2 (.send "g" "carol before")
    discard <| runUntil world fun n => says n 0 "g" "carol before" && says n 1 "g" "carol before"
    -- Alice stops reading Carol; Bob does not.
    let carol := ((chatOf (← get) 0 "g").bind fun (c : Chat) =>
      c.names.pairs.find? (fun (p : Key × String) => p.2 == "carol") |>.map (fun (p : Key × String) => p.1)).getD ByteArray.empty
    feed 0 (.remove "g" carol)
    let n ← get
    let gone := (chatOf n 0 "g").any fun (c : Chat) =>
      c.group.any (fun (g : Group) => g.readers.size == 1) && !c.log.any (·.text == "carol before")
    feed 2 (.send "g" "carol after")
    feed 1 (.send "g" "bob after")
    let bobHears ← runUntil world fun n => says n 1 "g" "carol after" && says n 0 "g" "bob after"
    let aliceDoesNot := !says (← get) 0 "g" "carol after"
    let unknown := (feed 0 (.remove "g" carol) *> get : NetM Net)
    let n ← unknown
    let refused := says n 0 "g" "that member is no longer in this group"
    -- Bob leaves; what was under way for the group comes to nothing.
    feed 1 (.leave "g")
    discard <| runUntil world (fun _ => false) 60000
    let left := (chatOf (← get) 1 "g").isNone
    return (gone, bobHears && aliceDoesNot, refused, left, true)).run net
  let (gone, others, refused, left, _) := ok
  check "remove stops reading a member and deletes what they sent" gone
  check "the others still read the removed member, who still reads them" others
  check "removing a member no longer there says so" refused
  check "leave deletes the group" left

def rename : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let (ok, _) ← (do
    unless ← foundAndJoin world do return (false, false, false, false, false)
    feed 0 (.send "g" "before")
    -- Renamed while that message and the reads of Bob's stream are under way.
    feed 0 (.rename "g" "friends")
    let n ← get
    let renamed := (chatOf n 0 "g").isNone && noticed n 0 "friends" "renamed from g"
    feed 0 (.create "g" none)
    let refused := (chatOf (← get) 0 "g").isNone &&
      noticed (← get) 0 "friends" "g was this group's name"
    feed 1 (.send "g" "from bob")
    let carried ← runUntil world fun n => says n 1 "g" "before" && says n 0 "friends" "from bob"
    feed 0 (.send "friends" "after")
    let after ← runUntil world fun n => says n 1 "g" "after"
    feed 0 (.rename "friends" "g")
    let back := (chatOf (← get) 0 "g").isSome
    return (renamed, refused, carried, after, back)).run net
  let (renamed, refused, carried, after, back) := ok
  check "rename gives a group another name here" renamed
  check "a name a group had until now is not taken by another" refused
  check "what was under way reaches the renamed group" carried
  check "a renamed group goes on talking" after
  check "a group can take its old name back" back

def restart : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let (ok, net) ← (do
    let j ← foundAndJoin world
    feed 0 (.send "g" "before")
    let r ← runUntil world fun n => says n 1 "g" "before"
    return j && r).run net
  check "a conversation before restarting" ok
  -- Alice stops, is saved, loads and carries on.
  let saved := save net.cores[0]!.state
  let loaded : Option State := load saved
  check "alice's state loads as saved" (loaded.isSome)
  let some s := loaded | return
  let (core, actions) := resume .plain s 1530
  let net := { net with cores := net.cores.set! 0 core,
                        queue := net.queue.filter (·.1 != 0) ++ actions.toArray.map (0, ·),
                        later := net.later.filter (·.2.1 != 0) }
  let (ok, _) ← (do
    feed 0 (.send "g" "after")
    feed 1 (.send "g" "bob after")
    runUntil world fun n => says n 1 "g" "after" && says n 0 "g" "bob after").run net
  check "alice carries on after restarting" ok

/-- Member `who` stops, losing what it had under way, and starts again from its saved state. -/
def restartMember (net : Net) (who : Nat) : Option Net := do
  let s : State ← load (save net.cores[who]!.state)
  let (core, actions) := resume .plain s 1530
  some { net with cores := net.cores.set! who core,
                  queue := net.queue.filter (·.1 != who) ++ actions.toArray.map (who, ·),
                  later := net.later.filter (·.2.1 != who) }

/-- Whether the next action is Alice's, publishing the reply to a voucher. -/
def publishingReply (n : Net) : Bool :=
  n.queue[0]?.any fun ((who, a) : Nat × Action) => who == 0 && match a with
    | .publishReply .. => true
    | _ => false

/-- Alice restarts while inducting Bob: before the voucher is read, and after the reply is
published but before she has heard so. Either way Bob joins, and Alice adds him. -/
def restartMidInduction : TestM Unit := do
  for (point, early) in [("before the voucher is read", true),
      ("after the reply is published", false)] do
    let (world, net) ← newNet 2 ["alice", "bob"]
    let (code, net) ← (show NetM (Option String) from do
      feed 0 (.create "g" none)
      feed 1 (.join "g" none)
      discard <| runUntil world fun n => (code? n 1 "g").isSome
      return code? (← get) 1 "g").run net
    let some code := code | check s!"bob mints a voucher ({point})" false
    let (ok, net) ← (show NetM Bool from do
      feed 0 (.induct "g" code)
      if early then return true
      unless ← runUntil world publishingReply do return false
      let n ← get
      let some (_, a) := n.queue[0]? | return false
      set { n with queue := n.queue.extract 1 n.queue.size }
      discard <| Store.withMemory ((n.files.lookup 0).getD []) (perform (backend world) · a)
      return true).run net
    check s!"alice gets to the point of restarting ({point})" ok
    let some net := restartMember net 0 | check s!"alice's state loads ({point})" false
    let (ok, _) ← (runUntil world fun n => joined n 1 "g" && noticed n 0 "g" "added ~bob").run net
    check s!"alice restarting {point} still adds bob" ok

def failingWrites : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let (ok, _) ← (do
    unless ← foundAndJoin world do return false
    world.modify fun w => { w with failWrites := 3 }
    feed 0 (.send "g" "eventually")
    runUntil world fun n => says n 1 "g" "eventually").run net
  check "a write that fails is tried again" ok

def backfill : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let (ok, _) ← (do
    unless ← foundAndJoin world do return (false, false)
    feed 0 (.send "g" "one")
    discard <| runUntil world fun n => says n 1 "g" "one"
    let before := (← world.get).writes.size
    -- A new replica epoch: each member rewrites its boxes, with the bytes first written.
    for e in [2, 3] do
      feed 0 (.epoch e)
      feed 1 (.epoch e)
      discard <| runUntil world fun n => n.cores.all fun c => c.state.chats.all (·.jobs.isEmpty)
    let w ← world.get
    let n ← get
    let rewrote := w.writes.size > before
    let idle := n.cores.all fun c => c.state.chats.all (·.jobs.isEmpty)
    return (rewrote, idle)).run net
  check "a new epoch rewrites boxes" ok.1
  check "every rewrite is accepted" ok.2

def nameTaken : TestM Unit := do
  let (world, net) ← newNet 1 ["alice"]
  let (ok, _) ← (do
    feed 0 (.create "fu" none)
    discard <| runUntil world fun n => joined n 0 "fu"
    feed 0 (.join "fu" none)
    feed 0 (.create "fu" none)
    let n ← get
    return n.cores[0]!.state.chats.length == 1 &&
      noticed n 0 "fu" "you already have a group called fu: to join" &&
      (chatOf n 0 "fu").any fun (c : Chat) => c.joining.isNone).run net
  check "the core refuses a name already used, and says why" ok

def persistState : TestM Unit := do
  let s : State := { me := "alice", chats := [{ name := "g", log := [{ id := 0, text := "x" }] }] }
  let back : Option State := load (save s)
  check "state saves and loads" (back.any fun b => b.me == "alice" && b.chats.length == 1)

def tests : TestM Unit := do
  base64
  persistState
  nameTaken
  twoMembers
  ownNames
  threeMembers
  removeAndLeave
  rename
  restart
  restartMidInduction
  failingWrites
  backfill

end GroupChatClient.GroupChatKatzenqt1.Tests.CoreTests
