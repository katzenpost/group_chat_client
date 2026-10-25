/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import GroupChatClient.GroupChatKatzenqt1.Shell
import GroupChatClient.GroupChatKatzenqt1.Sim
import GroupChatClient.Tests.Util

/-! # Saving before going out

The driver over the in-memory Pigeonhole, saving to a directory on the disk. -/

namespace GroupChatClient.GroupChatKatzenqt1.Tests.ShellTests

open GroupChatClient GroupChatClient.Tests
open GroupChatClient.GroupChatKatzenqt1.Persist

/-- A shell saving to `statePath`, whose backend counts the streams it is asked to make. -/
def shell (statePath : System.FilePath) (made : IO.Ref Nat) : IO Shell := do
  let b := Sim.backend (← IO.mkRef ({} : Sim.World))
  pure {
    backend := { b with newKeypair := do made.modify (· + 1); b.newKeypair }
    core := ← IO.mkRef { present := .plain, state := { me := "alice" } }
    inbox := ← Std.CloseableChannel.new
    log := Logger.Logger.off
    statePath
    display := fun _ => pure ()
    stop := ← IO.mkRef false
    app := "test" }

/-- Drive `ms` through `sh` and give what they launched up to a second to start. -/
def drive (sh : Shell) (ms : List Msg) (made : IO.Ref Nat) : IO Unit := do
  for m in ms do sh.post m
  discard <| EIO.toBaseIO sh.inbox.close
  sh.drive
  for _ in [0:20] do
    if (← made.get) > 0 then return
    IO.sleep 50

def tests : TestM Unit := do
  IO.FS.withTempDir fun dir => do
    -- A file written whole, by the user alone, with nothing left beside it.
    let path := dir / "file"
    replacePrivate path "first".toUTF8
    replacePrivate path "second".toUTF8
    let back ← IO.FS.readBinFile path
    check "a file replaced holds what was written last" (back == "second".toUTF8)
    check "no temporary file is left" !(← (path.addExtension "tmp").pathExists)
    let missing ← try fsync (dir / "missing"); pure false catch _ => pure true
    check "syncing what is not there fails" missing
    -- A step whose state cannot be saved launches nothing, and stops the shell.
    let made ← IO.mkRef 0
    let sh ← shell (dir / "no-such-dir" / "state") made
    drive sh [.create "g" none] made
    let n ← made.get
    check "a step not saved makes no stream" (n == 0) fun _ => s!"{n} made"
    check "a step not saved stops the shell" (← sh.stop.get)
    check "a step not saved leaves the core as it was" (← sh.core.get).state.chats.isEmpty
    -- A step that is saved goes out once the state is on the disk.
    let made ← IO.mkRef 0
    let sh ← shell (dir / "state") made
    drive sh [.create "g" none] made
    check "a step saved makes its stream" ((← made.get) == 1)
    let saved : Option State := load (← IO.FS.readBinFile (dir / "state"))
    check "the saved state has the group" (saved.any (·.chats.any (·.name == "g")))
    check "the shell keeps going" !(← sh.stop.get)
