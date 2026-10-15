/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import GroupChatClient.GroupChatKatzenqt1.Tests.Core
import GroupChatClient.GroupChatKatzenqt1.Tests.State
import GroupChatClient.GroupChatKatzenqt1.Tests.Scenario

/-! # The tests, in one process, with no daemon -/

open GroupChatClient.Tests
open GroupChatClient.GroupChatKatzenqt1.Tests

def main : IO UInt32 := do
  let mut ok := true
  for (name, t) in [("katzenqt1 core", CoreTests.tests), ("katzenqt1 state", StateTests.tests),
      ("katzenqt1 scenarios", Scenario.tests)] do
    ok := (← runSuite name t) && ok
  return if ok then 0 else 1
