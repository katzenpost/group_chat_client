/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

namespace GroupChatClient.Tests

abbrev TestM := StateT (Nat × Nat) IO

def check (name : String) (ok : Bool) (detail : Unit → String := fun _ => "") : TestM Unit := do
  let (pass, fail) ← get
  if ok then set (pass + 1, fail)
  else
    let d := detail ()
    IO.println s!"FAIL {name}{if d.isEmpty then "" else ": " ++ d}"
    set (pass, fail + 1)

def checkEq {α : Type} [BEq α] [Repr α] (name : String) (expected actual : α) : TestM Unit :=
  check name (expected == actual) fun _ => s!"expected {repr expected}, got {repr actual}"

def runSuite (name : String) (t : TestM Unit) : IO Bool := do
  let ((), (pass, fail)) ← t.run (0, 0)
  IO.println s!"{name}: {pass} passed, {fail} failed"
  return fail == 0

end GroupChatClient.Tests
