/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import GroupChatClient.GroupChatKatzenqt1.Core
import GroupChatClient.Tests.Util

/-! # Times

Lines saved with their times, and lines saved before times were kept; the core stamping what
each step adds. -/

namespace GroupChatClient.GroupChatKatzenqt1.Tests.StateTests

open GroupChatClient GroupChatClient.Tests
open Cryptlib.Protocols.GroupComm.Common
open Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member
open GroupChatClient.GroupChatKatzenqt1.Persist
open GroupChatClient.GroupChatKatzenqt1.Persist.Persist

def bob : Key := ByteArray.mk #[2]
def tests : TestM Unit := do
  -- Saving.
  let l : Line := { id := 7, sender := bob, text := "hi", time := some 1792055520000 }
  let back : Option Line := ofTerm (toTerm l)
  check "a line saves and loads with its time" (back.any fun b => b.time == some 1792055520000 && b.text == "hi")
  let old := match toTerm l with
    | .TList fs => Cborg.Term.TList (fs.extract 0 6)
    | t => t
  let oldBack : Option Line := ofTerm old
  check "a line saved before times were kept loads, with none" (oldBack.any fun b => b.time.isNone && b.id == 7)
  let s : State := { me := "alice", chats := [{ name := "g", log := [l] }] }
  check "a state with times saves and loads" ((load (save s) : Option State).any fun b =>
    b.chats.any fun c => c.log.any (·.time == some 1792055520000))
  let sealed : Inducted := { name := "carol", mutatedReadCap := bob, sealedReply := bob }
  let i : Inducting := { voucher := bob, payload := bob, sealed := some sealed }
  let iBack : Option Inducting := ofTerm (toTerm i)
  check "an induction saves and loads with its sealed reply" (iBack.any fun b =>
    b.sealed.any (·.name == "carol") && b.payload.data == bob.data)
  let oldI := match toTerm i with
    | .TList fs => Cborg.Term.TList (fs.extract 0 6)
    | t => t
  let oldIBack : Option Inducting := ofTerm oldI
  check "an induction saved before sealed replies were kept loads, with none" (oldIBack.any fun b =>
    b.sealed.isNone && b.voucher.data == bob.data)
  -- Stamping.
  let core : Core := { present := .plain, state := { me := "alice" }, now := 1000 }
  let (core, _) := step core (.create "g")
  -- A second group of that name is refused, with a notice: the line this step adds.
  let (core, _) := step { core with now := 2000 } (.create "g")
  check "what a step adds is stamped with the time now" ((core.state.chat? "g").any fun c =>
    c.log.length == 1 && c.log.all (·.time == some 2000))
  let (core, _) := step { core with now := 3000 } (.create "g")
  check "and what was there keeps its time" ((core.state.chat? "g").any fun c =>
    (c.log.map (·.time)) == [some 2000, some 3000])

end GroupChatClient.GroupChatKatzenqt1.Tests.StateTests
