/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member.Walk
import GroupChatClient.Pigeonhole.Box
import GroupChatClient.Shell
import GroupChatClient.GroupChatKatzenqt1.Core

/-! # Carrying out an action

`perform` does one `Action` on a `Backend` and says what came of it: a message for the core, or
that it is to be tried again later. Waiting is left to the caller, so that the app can sleep and a
test can step a simulated clock. -/

namespace GroupChatClient.GroupChatKatzenqt1

open Cryptlib.Protocols.GroupComm.Common
open Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member
open GroupChatClient.Pigeonhole (publish retryMs startOfWriteCap)

/-- Files under the state directory, by name relative to it. -/
structure Store where
  write : String → ByteArray → IO Unit
  read : String → IO ByteArray
  remove : String → IO Unit

/-- Files under `dir`, readable by the user alone. -/
def Store.dir (dir : System.FilePath) : Store where
  write name bytes := do
    let mut at_ := dir
    for part in (name.splitOn "/").dropLast do
      at_ := at_ / part
      unless ← at_.isDir do
        IO.FS.createDir at_
        IO.setAccessRights at_ { user := { read := true, write := true, execution := true } }
    replacePrivate (dir / name) bytes
  read name := IO.FS.readBinFile (dir / name)
  remove name := do
    if ← (dir / name).pathExists then IO.FS.removeFile (dir / name)

/-- Files in memory; for tests. -/
def Store.memory (files : IO.Ref (List (String × ByteArray))) : Store where
  write name bytes := files.modify fun fs => (name, bytes) :: fs.filter (·.1 != name)
  read name := do
    match (← files.get).lookup name with
    | some b => pure b
    | none => throw (IO.userError s!"no file {name}")
  remove name := files.modify (·.filter (·.1 != name))

/-- Run `x` with a store in memory holding `files`; what it did, and the files after. -/
def Store.withMemory {α : Type} (files : List (String × ByteArray)) (x : Store → IO α) :
    IO (α × List (String × ByteArray)) := do
  let ref ← IO.mkRef files
  let a ← x (Store.memory ref)
  return (a, ← ref.get)

inductive Outcome
  | done (m : Msg)
  /-- Try the action again after `ms` milliseconds. -/
  | retry (a : Action) (ms : Nat)
  /-- Hand the core `m` after `ms` milliseconds. -/
  | delay (m : Msg) (ms : Nat)

def describe : Action → String
  | .create chat joining => s!"{chat}: new stream{if joining then " to join with" else ""}"
  | .write chat job _ index box => s!"{chat}: write job {job} to box {counter index} ({box.size} bytes)"
  | .read chat _ _ index => s!"{chat}: read box {counter index}"
  | .wake chat _ ms => s!"{chat}: read again in {ms} ms"
  | .scanRead chat _ _ index => s!"{chat}: scan reads box {counter index}"
  | .walk chat _ start targets _ => s!"{chat}: walk from {counter start} to {targets}"
  | .derive chat _ start n => s!"{chat}: derive {n} indexes after {counter start}"
  | .mint chat .. => s!"{chat}: mint a voucher"
  | .awaitReply chat _ _ box1 _ => s!"{chat}: wait for the voucher's reply in box {counter box1}"
  | .readVoucher chat _ => s!"{chat}: read a voucher's box 0"
  | .induct chat .. => s!"{chat}: seal the reply to a voucher"
  | .file chat _ bytes => s!"{chat}: {(bytes.map (s!"keep a file of {·.size} bytes")).getD "delete a file"}"
  | .load chat job _ => s!"{chat}: read job {job}'s message back"
  | .substream chat job => s!"{chat}: new substream for job {job}"
  | .writeChunk chat job n _ index box =>
    s!"{chat}: write box {n} of job {job} to substream box {counter index} ({box.size} bytes)"
  | .chunkIndexes chat job _ n => s!"{chat}: derive {n} indexes of job {job}'s substream"
  | .readChunk chat _ _ index => s!"{chat}: read substream box {counter index}"

/-- Write `bytes` to the file `name`, or delete it when `none`: what came of it. -/
def keepFile (st : Store) (chat name : String) (bytes : Option ByteArray) : IO Msg := do
  try
    match bytes with
    | some b => st.write name b
    | none => st.remove name
    return .filed chat name none
  catch e => return .filed chat name (some (toString e))

/-- Write the files a step asks for, before the state it led to is saved, so that no saved state
names a file not yet there: what came of them, and the step's other actions. A file is deleted
only once a state no longer naming it is saved, so deletes stay with the others. -/
def writeFirst (st : Store) (actions : List Action) : IO (List Msg × List Action) := do
  let mut filed := []
  let mut rest := []
  for a in actions do
    match a with
    | .file chat name (some bytes) => filed := filed ++ [← keepFile st chat name (some bytes)]
    | a => rest := rest ++ [a]
  return (filed, rest)

def perform (b : Backend) (st : Store) (a : Action) : IO Outcome := do
  try
    match a with
    | .create chat joining => return .done (.created chat joining (← b.newKeypair))
    | .write chat job wc index box =>
      try return .done (.wrote chat job index (← b.writeBox wc index box))
      catch _ => return .retry a retryMs
    | .read chat stream rc index => return .done (.read chat stream index (← b.readBox rc index))
    | .wake chat stream ms => return .delay (.wake chat stream) ms
    | .scanRead chat stream rc index =>
      return .done (.scanRead chat stream index (← b.readBox rc index))
    | .walk chat stream start targets keep =>
      let (target, skipped, steps) ← walkWith b.nextIndex start targets keep
      return .done (.walked chat stream target skipped steps)
    | .derive chat stream start count =>
      return .done (.derived chat stream (← deriveWith b.nextIndex start count))
    | .mint chat wc name =>
      let m ← b.voucherMint wc name
      let box1 ← publish b.toBackend m.writeCap (startOfWriteCap m.writeCap) m.payload
      return .done (.minted chat m box1)
    | .awaitReply chat sk vrc box1 wc =>
      match ← b.readBox vrc box1 with
      | .data sealed _ => return .done (.opened chat (← b.voucherOpen sk sealed wc))
      | .notFound => return .retry a retryMs
      | .tombstone _ => return .done (.failed chat "the voucher's reply was withdrawn")
    | .readVoucher chat voucher =>
      let (vwc, vrc) ← b.voucherDeriveStream voucher
      match ← b.readBox vrc (startOf vrc) with
      | .data payload box1 => return .done (.joinerPayload chat voucher vwc payload box1)
      | .notFound => return .retry a retryMs
      | .tombstone _ => return .done (.failed chat "that voucher was withdrawn")
    | .induct chat voucher payload who vwc box1 =>
      let i ← b.voucherInduct voucher payload who
      discard <| publish b.toBackend vwc box1 i.sealedReply
      return .done (.inducted chat i)
    | .file chat name bytes => return .done (← keepFile st chat name bytes)
    | .load chat job name =>
      try return .done (.loaded chat job (some (← st.read name)))
      catch _ => return .done (.loaded chat job none)
    | .substream chat job => return .done (.substream chat job (← b.newKeypair))
    | .writeChunk chat job n wc index box =>
      try return .done (.wroteChunk chat job n (← b.writeBox wc index box))
      catch _ => return .retry a retryMs
    | .chunkIndexes chat job start count =>
      return .done (.chunkIndexes chat job start (← deriveWith b.nextIndex start count))
    | .readChunk chat stream rc index =>
      return .done (.readChunk chat stream index (← b.readBox rc index))
  catch e =>
    let chat := match a with
      | .create c .. | .write c .. | .read c .. | .wake c .. | .scanRead c .. | .walk c ..
      | .derive c .. | .mint c .. | .awaitReply c .. | .readVoucher c .. | .induct c ..
      | .file c .. | .load c .. | .substream c .. | .writeChunk c .. | .chunkIndexes c .. | .readChunk c .. => c
    match a with
    | .read .. | .scanRead .. | .readChunk .. | .substream .. | .chunkIndexes .. => return .retry a retryMs
    | _ => return .done (.failed chat s!"{describe a} failed: {e}")

end GroupChatClient.GroupChatKatzenqt1
