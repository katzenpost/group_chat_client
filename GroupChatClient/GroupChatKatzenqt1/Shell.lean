/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import Logger
import Std.Sync.Channel
import GroupChatClient.Shell
import GroupChatClient.GroupChatKatzenqt1.Exec

/-! # Running a member

One task owns the core: it takes each message in turn, steps the core, saves the state, shows it,
and launches the network work the step asks for, each on a task of its own that hands back what
came of it. Another task watches the replica epoch. -/

namespace GroupChatClient.GroupChatKatzenqt1

open CryptWalker.Protocols.GroupComm.Common
open CryptWalker.Protocols.GroupComm.GroupChatKatzenqt1.Member
open GroupChatClient.GroupChatKatzenqt1.Persist
open Logger

def Msg.describe : Msg → String
  | .create chat => s!"{chat}: create"
  | .join chat => s!"{chat}: join"
  | .induct chat _ => s!"{chat}: add a member"
  | .send chat text => s!"{chat}: send {text.length} characters"
  | .sendFile chat _ bytes => s!"{chat}: send a file of {bytes.size} bytes"
  | .scan chat => s!"{chat}: scan"
  | .leave chat => s!"{chat}: leave"
  | .remove chat _ => s!"{chat}: remove a member"
  | .clear chat => s!"{chat}: clear"
  | .rename chat to => s!"{chat}: rename to {to}"
  | .created chat joining _ => s!"{chat}: created a stream{if joining then " to join with" else ""}"
  | .wrote chat job index _ => s!"{chat}: wrote job {job} at box {counter index}"
  | .read chat stream index o =>
    let what := match o with
      | .data p _ => s!"{p.size} bytes"
      | .notFound => "nothing yet"
      | .tombstone _ => "a tombstone"
    s!"{chat}: read {short stream} box {counter index}: {what}"
  | .wake chat stream => s!"{chat}: read {short stream} again"
  | .scanRead chat stream index _ => s!"{chat}: scan read {short stream} box {counter index}"
  | .walked chat stream _ skipped steps =>
    s!"{chat}: walked {short stream} {steps} steps, {skipped.length} kept"
  | .derived chat stream indexes => s!"{chat}: derived {indexes.length} indexes of {short stream}"
  | .minted chat _ box1 => s!"{chat}: minted a voucher; its reply goes to box {counter box1}"
  | .opened chat _ => s!"{chat}: opened the voucher's reply"
  | .joinerPayload chat _ _ _ _ => s!"{chat}: read a voucher's box 0"
  | .inducted chat i => s!"{chat}: sealed the reply for {i.name.length}-character name"
  | .epoch n => s!"replica epoch {n}"
  | .failed chat what => s!"{chat}: {what}"
  | .filed chat _ error => s!"{chat}: {(error.map ("a file could not be kept: " ++ ·)).getD "kept a file"}"
  | .loaded chat job bytes => s!"{chat}: job {job}'s message: {(bytes.map (s!"{·.size} bytes")).getD "lost"}"
  | .substream chat job _ => s!"{chat}: made a substream for job {job}"
  | .wroteChunk chat job n _ => s!"{chat}: wrote box {n} of job {job}"
  | .chunkIndexes chat job _ is => s!"{chat}: derived {is.length} indexes of job {job}'s substream"
  | .readChunk chat stream index o =>
    let what := match o with
      | .data p _ => s!"{p.size} bytes"
      | .notFound => "nothing yet"
      | .tombstone _ => "a tombstone"
    s!"{chat}: read a substream of {short stream} at box {counter index}: {what}"

structure Shell where
  backend : Backend
  core : IO.Ref Core
  inbox : Std.CloseableChannel Msg
  log : Logger
  statePath : System.FilePath
  /-- Called with the state after each step. -/
  display : State → IO Unit
  stop : IO.Ref Bool
  /-- The app's name, before each log module's. -/
  app : String

namespace Shell

def post (sh : Shell) (m : Msg) : IO Unit := discard <| sh.inbox.send m

/-- Sleep `ms` milliseconds, waking early when the shell stops; whether it is still running. -/
def nap (sh : Shell) (ms : Nat) : IO Bool := GroupChatClient.nap sh.stop ms

partial def launch (sh : Shell) (a : Action) : IO Unit := do
  let net := sh.log.withModule s!"{sh.app}/net"
  discard <| IO.asTask (prio := .dedicated) do
    net.debug (describe a)
    let store := Store.dir (sh.statePath.parent.getD ".")
    match ← perform sh.backend store a with
    | .done m => sh.post m
    | .retry a ms =>
      net.info s!"{describe a}: trying again in {ms} ms"
      if ← nap sh ms then launch sh a
    | .delay m ms => if ← nap sh ms then sh.post m

def save (sh : Shell) (s : State) : IO Unit := replacePrivate sh.statePath (Persist.save s)

/-- Write each voucher code we mint to `voucher-GROUP.txt` beside the state, readable by the user
alone, so that it can be copied whole however narrow the screen. -/
def writeVouchers (sh : Shell) (s : State) : IO Unit := do
  let some dir := sh.statePath.parent | return
  for c in s.chats do
    let some (.awaiting code ..) := c.joining | continue
    let path := dir / voucherFile c.name
    if ← path.pathExists then continue
    let h ← IO.FS.Handle.mk path .write
    IO.setAccessRights path private_
    h.putStrLn code
    (sh.log.withModule s!"{sh.app}/store").notice s!"{c.name}: the voucher code is in {path}"

/-- Take messages until the shell stops. -/
partial def drive (sh : Shell) : IO Unit := do
  let log := sh.log.withModule s!"{sh.app}/driver"
  let some m ← IO.wait (← sh.inbox.recv) | return
  if ← sh.stop.get then return
  log.debug m.describe
  if let .failed _ what := m then log.warning what
  let (c, actions) := step { (← sh.core.get) with now := ← wallMs } m
  sh.core.set c
  try
    sh.save c.state
    sh.writeVouchers c.state
  catch e => (sh.log.withModule s!"{sh.app}/store").error s!"saving: {e}"
  sh.display c.state
  for a in actions do launch sh a
  drive sh

/-- Tell the core the replica epoch each minute. -/
partial def watchEpoch (sh : Shell) : IO Unit := do
  try
    if let some n ← sh.backend.replicaEpoch then sh.post (.epoch n)
  catch e => (sh.log.withModule s!"{sh.app}/net").warning s!"replica epoch: {e}"
  if ← nap sh 60000 then watchEpoch sh

/-- Start the driver and the epoch watcher, and whatever the saved state had under way. -/
def start (sh : Shell) (resumed : List Action) : IO Unit := do
  discard <| IO.asTask (prio := .dedicated) sh.drive
  discard <| IO.asTask (prio := .dedicated) sh.watchEpoch
  for a in resumed do launch sh a

end Shell

end GroupChatClient.GroupChatKatzenqt1
