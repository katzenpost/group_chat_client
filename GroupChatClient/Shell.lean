/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import Std.Time
import LeanOs

/-! # What every shell needs: a clock, sleeping until stopped, and private files -/

namespace GroupChatClient

/-- Milliseconds since the Unix epoch, by this machine's clock. -/
def wallMs : IO Nat := do
  return (← Std.Time.Timestamp.now).toMillisecondsSinceUnixEpoch.val.toNat

def hex (b : ByteArray) : String :=
  String.join (b.data.toList.map fun x =>
    let s := String.ofList (Nat.toDigits 16 x.toNat)
    if s.length < 2 then "0" ++ s else s)

/-- The first bytes of `b` in hex, enough to tell streams apart in a log. -/
def short (b : ByteArray) : String := hex (b.extract 0 4)

/-- Sleep `ms` milliseconds, waking early once `stop` is set; whether it is still unset. -/
partial def nap (stop : IO.Ref Bool) (ms : Nat) : IO Bool := do
  if ← stop.get then return false
  if ms = 0 then return true
  let slice := min ms 500
  IO.sleep slice.toUInt32
  nap stop (ms - slice)

/-- Readable and writable by the user alone. -/
def private_ : IO.FileRight := { user := { read := true, write := true } }

/-- Write `bytes` to a file only the user may read, which then takes the place of `path`: on the
disk, whole, by the time this returns. -/
def replacePrivate (path : System.FilePath) (bytes : ByteArray) : IO Unit := do
  let tmp := path.addExtension "tmp"
  let h ← IO.FS.Handle.mk tmp .write
  IO.setAccessRights tmp private_
  h.write bytes
  h.flush
  LeanOs.fsync tmp
  IO.FS.rename tmp path
  LeanOs.fsync <| match path.parent with
    | some dir => if dir.toString.isEmpty then "." else dir
    | none => "."

end GroupChatClient
