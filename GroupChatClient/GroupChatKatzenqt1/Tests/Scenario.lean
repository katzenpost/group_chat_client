/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import GroupChatClient.GroupChatKatzenqt1.Shell
import GroupChatClient.GroupChatKatzenqt1.Sim
import GroupChatClient.Tests.Util

/-! # Scenarios for the client core

Members, each a client core, over the simulated Pigeonhole, driven by a scenario: a text file of
commands, one per line, as katzenqt's group chat scenarios are written. A command followed by a
`----` line has its output checked against the lines after it, up to a blank line; `#` starts a
comment.

The network runs on a logical clock, and only when told to: `run` carries out what the members
asked for, in order, and lets time pass. So a scenario sets up a race exactly: what each member
has asked for before a `run` happens in the same stretch of time, and a member whose traffic is
held asks and hears nothing until it is released.

| Command | |
|---|---|
| `create WHO GROUP` | WHO founds a group |
| `join WHO GROUP` | WHO mints a voucher to join a group they call GROUP |
| `invite WHO GROUP NEW [THEIRS]` | WHO adds NEW with the voucher NEW minted for THEIRS (default GROUP) |
| `send WHO GROUP TEXT…` | WHO sends TEXT |
| `remove WHO GROUP GONE`, `scan WHO GROUP`, `leave WHO GROUP` | as a member asks |
| `run [SECONDS]` | the network runs for SECONDS of logical time, 600 by default |
| `hold WHO`, `release WHO` | WHO's traffic stops, or goes again |
| `epoch` | the replicas' epoch passes, and every member is told |
| `fail-writes N` | the next N box writes fail |
| `show WHO GROUP` | WHO's conversation: notices after `—`, messages after their sender, ours with `…`, `✓` or `✓✓` |
| `members WHO GROUP` | whom WHO reads in the group |
| `acked WHO GROUP` | how far each member has acknowledged WHO's stream, as WHO credits it |
| `state WHO GROUP` | whether WHO has joined, and what is under way |
| `jobs WHO GROUP` | what WHO has still to write, in order |
| `refused` | the boxes whose writes the replicas refused, holding other bytes | -/

namespace GroupChatClient.GroupChatKatzenqt1.Tests.Scenario

open GroupChatClient
open CryptWalker.Protocols.GroupComm.Common
open CryptWalker.Protocols.GroupComm.GroupChatKatzenqt1.Member
open GroupChatClient.Pigeonhole (startOfWriteCap readCapOf)

inductive Later
  | act (a : Action)
  | msg (m : Msg)

structure Member where
  name : String
  core : Core
  deriving Inhabited

structure World where
  sim : IO.Ref Sim.World
  members : Array Member := #[]
  queue : Array (Nat × Action) := #[]
  later : Array (Nat × Nat × Later) := #[]
  clock : Nat := 0
  held : List Nat := []
  epoch : Nat := 1
  /-- Each member's files. -/
  files : List (Nat × List (String × ByteArray)) := []
  /-- Each step after which a member's group did not load as it was saved. -/
  unsaved : Array String := #[]

abbrev ScM := StateT World IO

def who (name : String) : ScM Nat := do
  let w ← get
  match w.members.findIdx? (·.name == name) with
  | some i => return i
  | none =>
    set { w with members := w.members.push { name, core := { present := .plain, state := { me := name } } } }
    return w.members.size

def feed (i : Nat) (m : Msg) : ScM Unit := do
  let w ← get
  let (c, as) := step w.members[i]!.core m
  let lost := c.state.chats.filter fun ch => ch.group.any fun g => Persist.load (Persist.save g) != some g
  set { w with members := w.members.modify i ({ · with core := c }),
               queue := w.queue ++ as.toArray.map (i, ·),
               unsaved := w.unsaved ++ lost.toArray.map fun (ch : Chat) =>
                 s!"{w.members[i]!.name} {ch.name} after {m.describe}" }

/-- The member whose key `k` is: the truth, for printing. -/
def nameOf (k : Key) : ScM String := do
  for m in (← get).members do
    for c in m.core.state.chats do
      if c.group.any (·.me == k) then return m.name
  return "?"

/-- Carry out the first action of a member not held; `false` if there is none. -/
def act : ScM Bool := do
  let w ← get
  let some k := w.queue.findIdx? (!w.held.contains ·.1) | return false
  let some (i, a) := w.queue[k]? | return false
  set { w with queue := w.queue.eraseIdx! k }
  let (outcome, files) ← Store.withMemory ((w.files.lookup i).getD []) (perform (Sim.backend w.sim) · a)
  modify fun w => { w with files := (i, files) :: w.files.filter (·.1 != i) }
  match outcome with
  | .done m => feed i m
  | .retry a ms => modify fun w => { w with later := w.later.push (w.clock + ms, i, .act a) }
  | .delay m ms => modify fun w => { w with later := w.later.push (w.clock + ms, i, .msg m) }
  return true

/-- Let the network run until `stop` on the logical clock. -/
partial def runTo (stop : Nat) : ScM Unit := do
  if ← act then return ← runTo stop
  let w ← get
  let due := w.later.filter fun (t, i, _) => t ≤ stop && !w.held.contains i
  match (due.map (·.1)).toList.min? with
  | none => set { w with clock := max w.clock stop }
  | some t =>
    let (now, rest) := w.later.partition fun (t', i, _) => t' ≤ t && !w.held.contains i
    set { w with later := rest, clock := max w.clock t }
    for (_, i, l) in now do
      match l with
      | .act a => modify fun w => { w with queue := w.queue.push (i, a) }
      | .msg m => feed i m
    runTo stop

def chatOf (i : Nat) (group : String) : ScM (Option Chat) :=
  return (← get).members[i]!.core.state.chat? group

def mark (c : Chat) (l : Line) : String :=
  match l.status with
  | .pending => " …"
  | .sent => if acknowledged c l then " ✓✓" else " ✓"

def showChat (i : Nat) (group : String) : ScM (List String) := do
  let some c ← chatOf i group | return ["no such group"]
  c.log.mapM fun l => do
    if l.sender.size == 0 then return s!"— {l.text}"
    let mine := c.group.any (·.me == l.sender)
    return s!"{← nameOf l.sender}{if mine then mark c l else ""}: {l.text.replace "\n" " | "}"

def membersOf (i : Nat) (group : String) : ScM (List String) := do
  let some c ← chatOf i group | return ["no such group"]
  let some g := c.group | return ["not joined"]
  let names ← g.readers.keys.mapM nameOf
  return [String.intercalate " " (names.mergeSort (· ≤ ·)) |>.trimAscii.toString |> fun s => if s.isEmpty then "nobody" else s]

/-- How far each member has acknowledged our stream: their name and the box, counted from our
stream's first. -/
def ackedOf (i : Nat) (group : String) : ScM (List String) := do
  let some c ← chatOf i group | return ["no such group"]
  let some g := c.group | return ["not joined"]
  let first := counter (startOfWriteCap c.writeCap)
  let acks ← g.ledger.ackedBy.pairs.mapM fun (k, n) => return s!"{← nameOf k}:{n - first}"
  return [if acks.isEmpty then "none" else String.intercalate " " (acks.mergeSort (· ≤ ·))]

def stateOf (i : Nat) (group : String) : ScM (List String) := do
  let some c ← chatOf i group | return ["no such group"]
  let parts := [if c.group.isSome then "joined" else "not joined"] ++
    (match c.joining with
      | some .minting => ["minting"]
      | some (.awaiting ..) => ["awaiting a reply"]
      | none => []) ++
    (if c.inducting.isSome then ["adding a member"] else []) ++
    (if c.jobs.isEmpty then [] else [s!"{c.jobs.length} to write"])
  return [String.intercalate ", " parts]

/-- What WHO has still to write, in order: the box, from the stream's first, and whether it is a
message of ours or a rewrite the member asked for. -/
def jobsOf (i : Nat) (group : String) : ScM (List String) := do
  let some c ← chatOf i group | return ["no such group"]
  let first := counter (startOfWriteCap c.writeCap)
  let job (j : Job) : String :=
    let at_ := (j.index.map fun x => s!"box {counter x - first}").getD "next box"
    let what := if j.token.isSome then "message" else if j.box.size == 0 then "tombstone" else "rewrite"
    s!"{what} to {at_}, {j.box.size} bytes"
  return if c.jobs.isEmpty then ["none"] else c.jobs.map job

/-- Writes the replicas refused, each to a box holding other bytes: whose stream, and which box. -/
def refusedOf : ScM (List String) := do
  let w ← get
  let sim ← w.sim.get
  let names ← sim.refused.toList.mapM fun (key, n) => do
    let mut name := "?"
    for m in w.members do
      for c in m.core.state.chats do
        if c.writeCap.size > 0 && (keyOf (readCapOf c.writeCap)).data.toList == key then
          name := m.name
    return s!"{name}@{n}"
  return if names.isEmpty then ["none"] else [String.intercalate " " names.eraseDups]

def codeOf (i : Nat) (group : String) : ScM (Option String) := do
  match (← chatOf i group).bind (·.joining) with
  | some (.awaiting code ..) => return some code
  | _ => return none

def command (words : List String) : ScM (List String) := do
  match words with
  | ["create", w, g] => feed (← who w) (.create g); return []
  | ["join", w, g] => feed (← who w) (.join g); return []
  | "invite" :: w :: g :: new :: rest =>
    let some code ← codeOf (← who new) (rest.headD g) | return [s!"{new} has no voucher code"]
    feed (← who w) (.induct g code)
    return []
  | "send" :: w :: g :: text => feed (← who w) (.send g (" ".intercalate text)); return []
  | ["remove", w, g, gone] =>
    let i ← who w
    let some k := ((← chatOf (← who gone) g).bind (·.group)).map (·.me) | return [s!"{gone} is not in {g}"]
    feed i (.remove g k)
    return []
  | ["scan", w, g] => feed (← who w) (.scan g); return []
  | ["leave", w, g] => feed (← who w) (.leave g); return []
  | ["run"] => runTo ((← get).clock + 600000); return []
  | ["run", s] => runTo ((← get).clock + s.toNat! * 1000); return []
  | ["hold", w] =>
    let i ← who w
    modify fun s => { s with held := i :: s.held }
    return []
  | ["release", w] =>
    let i ← who w
    modify fun s => { s with held := s.held.erase i }
    return []
  | ["epoch"] =>
    let n := (← get).epoch + 1
    modify fun s => { s with epoch := n }
    (← get).sim.modify fun s => { s with epoch := some n }
    for i in List.range (← get).members.size do feed i (.epoch n)
    return []
  | ["fail-writes", n] => (← get).sim.modify fun s => { s with failWrites := n.toNat! }; return []
  | ["show", w, g] => showChat (← who w) g
  | ["members", w, g] => membersOf (← who w) g
  | ["acked", w, g] => ackedOf (← who w) g
  | ["state", w, g] => stateOf (← who w) g
  | ["jobs", w, g] => jobsOf (← who w) g
  | ["refused"] => refusedOf
  | _ => throw (IO.userError s!"not a command: {" ".intercalate words}")

/-- Run a scenario: the script with every checked command's output replaced by what it printed
now. -/
def run (script : String) : IO (String × Array String) := do
  let lines := (script.splitOn "\n").toArray
  let lines := if lines.back? == some "" then lines.pop else lines
  let world : World := { sim := ← IO.mkRef {} }
  let (out, _) ← (do
    let mut out : Array String := #[]
    let mut i := 0
    while i < lines.size do
      let line := lines[i]!
      out := out.push line
      i := i + 1
      let cmd := ((line.splitOn "#").headD "").trimAscii.toString
      if cmd.isEmpty then continue
      let printed ← command ((cmd.splitOn " ").filter (· ≠ ""))
      if i < lines.size && lines[i]! == "----" then
        out := out.push "----"
        i := i + 1
        while i < lines.size && !lines[i]!.trimAscii.isEmpty do i := i + 1
        out := out ++ printed.toArray
    return (out, (← get).unsaved) : ScM (Array String × Array String)).run world
  return ("\n".intercalate out.1.toList ++ "\n", out.2)

def dir : System.FilePath := "GroupChatClient/GroupChatKatzenqt1/Tests/scenarios"

/-- Every scenario in `dir`, each checked against its expected output; with
`SCENARIO_REWRITE=1`, the files take the output instead. -/
def tests : Tests.TestM Unit := do
  let rewrite := (← IO.getEnv "SCENARIO_REWRITE") == some "1"
  let files := ((← dir.readDir).map (·.path)).filter (·.extension == some "txt")
  for path in files.qsort (·.toString < ·.toString) do
    let script ← IO.FS.readFile path
    let (actual, unsaved) ← run script
    Tests.check s!"scenario {path.fileStem.getD ""}: each group loads as it was saved" unsaved.isEmpty
      fun _ => ", ".intercalate unsaved.toList
    if rewrite then IO.FS.writeFile path actual
    Tests.check s!"scenario {path.fileStem.getD ""}" (rewrite || actual == script) fun _ =>
      let a := actual.splitOn "\n"
      let e := script.splitOn "\n"
      match (a.zip e).zipIdx.find? (fun ((x, y), _) => x != y) with
      | some ((x, y), n) => s!"line {n + 1}: expected {repr y}, got {repr x}"
      | none => "the lengths differ"

end GroupChatClient.GroupChatKatzenqt1.Tests.Scenario
