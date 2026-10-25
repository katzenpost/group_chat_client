/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member.Gcm
import Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member.Membership
import Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member.ReplyWho
import GroupChatClient.Base64
import GroupChatClient.GroupChatKatzenqt1.Files
import GroupChatClient.GroupChatKatzenqt1.Presentation
import GroupChatClient.GroupChatKatzenqt1.Backend
import GroupChatClient.GroupChatKatzenqt1.State

/-! # What a member does

`step` takes the state and one message, from the user or from the network, and returns the
new state and the network work it asks for. It does no IO, so it can be tested on its own; the
shell carries out each `Action` and sends back what came of it as another `Msg`.

The member state machine is driven as katzenqt's adapter (`acks.py`) drives it: `compose` for
each message sent, `written` once its box is written, `observe` for each box read in order,
`handle` for a new epoch and for a user's scan, and for a new member `beginInduction`, then once
everything owed is written `reply`, then `introduceNow`. -/

namespace GroupChatClient.GroupChatKatzenqt1

open Cryptlib.Protocols.GroupComm.Common
open Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member
open GroupChatClient.Pigeonhole (Keypair ReadOutcome startOfWriteCap readCapOf)
open GroupChatClient.GroupChatKatzenqt1.Files (FileUpload)

/-- Network work. Each comes back as the `Msg` named. -/
inductive Action
  /-- A new stream: `created`. -/
  | create (chat : String) (joining : Bool)
  /-- Write a box, retrying until it is written: `wrote`. -/
  | write (chat : String) (job : Nat) (writeCap : ByteArray) (index : Index) (box : ByteArray)
  /-- Read a box once: `read`. -/
  | read (chat : String) (stream : Key) (readCap : ByteArray) (index : Index)
  /-- Send `wake` after `ms` milliseconds. -/
  | wake (chat : String) (stream : Key) (ms : Nat)
  /-- Read a box once for a scan: `scanRead`. -/
  | scanRead (chat : String) (stream : Key) (readCap : ByteArray) (index : Index)
  /-- `walked`. -/
  | walk (chat : String) (stream : Key) (start : Index) (targets : List Nat) (keep : Nat)
  /-- `derived`. -/
  | derive (chat : String) (stream : Key) (start : Index) (count : Nat)
  /-- Mint a voucher and publish its box 0: `minted`. -/
  | mint (chat : String) (writeCap : ByteArray) (name : String)
  /-- Wait for the reply in box 1 and open it: `opened`. -/
  | awaitReply (chat : String) (secretKey voucherReadCap : ByteArray) (box1 : Index)
      (writeCap : ByteArray)
  /-- Read a voucher's box 0: `joinerPayload`. -/
  | readVoucher (chat : String) (voucher : ByteArray)
  /-- Seal the reply: `sealed`. -/
  | seal (chat : String) (voucher payload whoReply : ByteArray)
  /-- Publish the sealed reply to box 1, retrying until it is written: `inducted`. -/
  | publishReply (chat : String) (voucherWriteCap : ByteArray) (box1 : Index) (i : Inducted)
  /-- Write `bytes` to the file `name` under the state directory, or delete it when `none`:
  `filed`. -/
  | file (chat : String) (name : String) (bytes : Option ByteArray)
  /-- Read the file holding an upload's message: `loaded`. -/
  | load (chat : String) (job : Nat) (name : String)
  /-- A new stream for an upload: `substream`. -/
  | substream (chat : String) (job : Nat)
  /-- Write the `n`th box of an upload, retrying until it is written: `wroteChunk`. -/
  | writeChunk (chat : String) (job : Nat) (n : Nat) (writeCap : ByteArray) (index : Index)
      (box : ByteArray)
  /-- The indexes of `count` boxes after `start` of an upload's substream: `chunkIndexes`. -/
  | chunkIndexes (chat : String) (job : Nat) (start : Index) (count : Nat)
  /-- Read a box of the substream a member's `I` box named: `readChunk`. -/
  | readChunk (chat : String) (stream : Key) (readCap : ByteArray) (index : Index)

inductive Msg
  /-- Found a group, where we go by `me`; with `none`, by the state's name. -/
  | create (chat : String) (me : Option String)
  /-- Join a group, where we go by `me`; with `none`, by the state's name. -/
  | join (chat : String) (me : Option String)
  | induct (chat : String) (code : String)
  | send (chat : String) (text : String)
  /-- Send a file, named `name`. -/
  | sendFile (chat : String) (name : String) (bytes : ByteArray)
  | scan (chat : String)
  /-- Forget a group: stop reading and writing it, and delete it. -/
  | leave (chat : String)
  /-- Stop reading a member, designated by key. -/
  | remove (chat : String) (who : Key)
  /-- Delete a conversation's lines here. -/
  | clear (chat : String)
  /-- Call a group something else here; the other members each know it by a name of their own. -/
  | rename (chat : String) (to : String)
  | created (chat : String) (joining : Bool) (kp : Keypair)
  | wrote (chat : String) (job : Nat) (index next : Index)
  | read (chat : String) (stream : Key) (index : Index) (outcome : ReadOutcome)
  | wake (chat : String) (stream : Key)
  | scanRead (chat : String) (stream : Key) (index : Index) (outcome : ReadOutcome)
  | walked (chat : String) (stream : Key) (target : Option Index) (skipped : List Index)
      (steps : Nat)
  | derived (chat : String) (stream : Key) (indexes : List Index)
  | minted (chat : String) (m : Minted) (box1 : Index)
  | opened (chat : String) (o : Opened)
  | joinerPayload (chat : String) (voucher voucherWriteCap payload : ByteArray) (box1 : Index)
  | sealed (chat : String) (i : Inducted)
  | inducted (chat : String) (i : Inducted)
  | epoch (n : Nat)
  | failed (chat : String) (what : String)
  | filed (chat : String) (name : String) (error : Option String)
  | loaded (chat : String) (job : Nat) (bytes : Option ByteArray)
  | substream (chat : String) (job : Nat) (kp : Keypair)
  | wroteChunk (chat : String) (job : Nat) (n : Nat) (next : Index)
  | chunkIndexes (chat : String) (job : Nat) (start : Index) (indexes : List Index)
  | readChunk (chat : String) (stream : Key) (index : Index) (outcome : ReadOutcome)

/-- The message about the group `f` names instead; `create` and `join` name a group not yet
there, and are kept. -/
def Msg.mapChat (f : String → String) : Msg → Msg
  | .induct c code => .induct (f c) code
  | .send c text => .send (f c) text
  | .sendFile c name bytes => .sendFile (f c) name bytes
  | .scan c => .scan (f c)
  | .leave c => .leave (f c)
  | .remove c who => .remove (f c) who
  | .clear c => .clear (f c)
  | .rename c to => .rename (f c) to
  | .created c joining kp => .created (f c) joining kp
  | .wrote c job index next => .wrote (f c) job index next
  | .read c stream index outcome => .read (f c) stream index outcome
  | .wake c stream => .wake (f c) stream
  | .scanRead c stream index outcome => .scanRead (f c) stream index outcome
  | .walked c stream target skipped steps => .walked (f c) stream target skipped steps
  | .derived c stream indexes => .derived (f c) stream indexes
  | .minted c m box1 => .minted (f c) m box1
  | .opened c o => .opened (f c) o
  | .joinerPayload c voucher vwc payload box1 => .joinerPayload (f c) voucher vwc payload box1
  | .sealed c i => .sealed (f c) i
  | .inducted c i => .inducted (f c) i
  | .failed c what => .failed (f c) what
  | .filed c name error => .filed (f c) name error
  | .loaded c job bytes => .loaded (f c) job bytes
  | .substream c job kp => .substream (f c) job kp
  | .wroteChunk c job n next => .wroteChunk (f c) job n next
  | .chunkIndexes c job start indexes => .chunkIndexes (f c) job start indexes
  | .readChunk c stream index outcome => .readChunk (f c) stream index outcome
  | m@(.create ..) | m@(.join ..) | m@(.epoch _) => m

/-- What is under way on the network; not saved, since a restart begins it again. -/
structure Flight where
  writing : List String := []
  /-- Streams with a read out or a wake pending. -/
  reading : List (String × Key) := []
  /-- Reads of each stream that found nothing, in a row. -/
  misses : List ((String × Key) × Nat) := []
  /-- Uploads with a substream being made or indexes being derived, by chat and job. -/
  uploading : List (String × Nat) := []
  /-- The index of each box of an upload from the first not yet written on, as far as derived. -/
  ahead : List ((String × Nat) × List Index) := []
  /-- The boxes of each upload being written. -/
  chunksOut : List ((String × Nat) × Nat) := []
  /-- Boxes of an upload written after one not yet written, with the index after each. -/
  landed : List ((String × Nat) × (Nat × Index)) := []
  /-- Uploads whose message is being read from its file. -/
  loading : List (String × Nat) := []
  /-- Each upload's message, once made or read back. -/
  blobs : List ((String × Nat) × ByteArray) := []
  /-- What each download has read: the index of its next box, and the boxes so far. -/
  fetched : List ((String × Key) × (Index × Array ByteArray)) := []
  /-- The bytes of a file a download just brought, by sender and box, while the member reads
  the message without them. -/
  arriving : List ((Key × Index) × ByteArray) := []
  /-- Each name a group had before it was renamed, with the name it has now, so that what comes
  back for work started under the old name reaches it. -/
  renamed : List (String × String) := []
  deriving Inhabited

/-- What is under way for the group `old`, as for `new`. -/
def Flight.rename (f : Flight) (old new : String) : Flight :=
  let n (c : String) := if c == old then new else c
  let k {α : Type} (p : String × α) := (n p.1, p.2)
  let kk {α β : Type} (p : (String × α) × β) := (k p.1, p.2)
  { f with writing := f.writing.map n, reading := f.reading.map k, misses := f.misses.map kk,
           uploading := f.uploading.map k, ahead := f.ahead.map kk, chunksOut := f.chunksOut.map kk,
           landed := f.landed.map kk, loading := f.loading.map k, blobs := f.blobs.map kk,
           fetched := f.fetched.map kk,
           renamed := (old, new) :: (f.renamed.filter (·.1 != new)).map fun (a, b) => (a, n b) }

/-- The name the group once called `name` has now. -/
def Flight.current (f : Flight) (name : String) : String := (f.renamed.lookup name).getD name

structure Core where
  state : State
  flight : Flight := {}
  maxPlaintext : Nat := 1530
  /-- The time now, in milliseconds since the Unix epoch, given by the shell before each step:
  each line a step adds is stamped with it. -/
  now : Nat := 0
  present : Presentation
  deriving Inhabited

abbrev CoreM := StateM (Core × Array Action)

namespace Core

def emit (a : Action) : CoreM Unit := modify fun (c, as) => (c, as.push a)

def getChat (name : String) : CoreM (Option Chat) := return (← get).1.state.chat? name

def setChat (c : Chat) : CoreM Unit :=
  modify fun (core, as) => ({ core with state := core.state.setChat c }, as)

def me : CoreM String := return (← get).1.state.me

/-- Our name in the group: the one we gave it, or else the state's. -/
def meIn (c : Chat) : CoreM String := return c.me.getD (← me)

def modifyFlight (f : Flight → Flight) : CoreM Unit :=
  modify fun (c, as) => ({ c with flight := f c.flight }, as)

def getFlight : CoreM Flight := return (← get).1.flight

/-- Run a member's step on `c`'s group. -/
def onGroup {α : Type} (c : Chat) (x : GM α) (default : α) : α × Chat :=
  match c.group with
  | none => (default, c)
  | some g => let (a, g) := x.run g; (a, { c with group := some g })

/-! ## Writing -/

/-- Every member's read cap, ourselves as one of them. -/
def memberCaps (g : Group) : List ByteArray := g.readCap :: g.readers.values.map (·.readCap)

/-- The box an outgoing message is written as. A message the member makes itself carries our
membership hash, an `Introduction` the hash before the member it introduces; content carries the
hash it was composed with. -/
def boxOf (g : Group) (o : Outgoing) : Option ByteArray := do
  let gcm ← Gcm.ofMessage o.message
  let gcm := if o.message.content.isSome then gcm else
    let caps := match o.message.introduction with
      | some i => (memberCaps g).filter fun c => keyOf c != keyOf i.readCap
      | none => memberCaps g
    { gcm with membershipHash := membershipHash caps }
  some (Gcm.frame (Gcm.encode gcm))

/-- The file under the state directory that holds the message of upload `job`. -/
def spoolFile (c : Chat) (job : Nat) (now : Nat) : String := s!"spool/{slug c.name}-{now}-{job}.cbor"

/-- Queue `outgoing`, each written after the one before; `line` shows the one carrying
`content`, into which the bytes of a file it carries, `payload`, are put. A message too long for
a box goes to a substream, written before the `I` box naming it. -/
def enqueue (c : Chat) (outgoing : List Outgoing) (content : Option ByteArray := none)
    (line : Option Nat := none) (payload : Option ByteArray := none) : CoreM Chat := do
  let core := (← get).1
  let max := core.maxPlaintext
  let mut c := c
  for o in outgoing do
    let some g := c.group | pure ()
    let mine := content.isSome && o.message.content.map (·.data) == content.map (·.data)
    let box := (boxOf g o).bind fun box =>
      match payload.filter fun _ => mine with
      | some p => (Files.putPayload (box.extract 1 box.size) p).map Gcm.frame
      | none => some box
    match box with
    | some box =>
      let line := if mine then line else none
      let (id, c') := c.fresh
      if box.size ≤ max then
        c := { c' with jobs := c'.jobs ++ [{ id, token := some o.token, box, line }] }
        continue
      let blob := box.extract 1 box.size
      if blob.size ≤ Files.maxFile + max then
        let file := spoolFile c id core.now
        emit (.file c.name file (some blob))
        modify fun (core, as) => ({ core with flight := { core.flight with
          blobs := ((c.name, id), blob) :: core.flight.blobs } }, as)
        let upload := { file, boxSize := max, total := Files.chunkCount blob.size max }
        c := { c' with jobs := c'.jobs ++ [{ id, token := some o.token, box := .empty, line,
                                             upload := some upload }] }
        continue
    | none => pure ()
    c := (onGroup c (Group.abandoned o.token) ()).2
    c := c.notice (if mine then "that message is too long to send" else "a message could not be written")
  return c

/-- A box the member asks to write again, or to tombstone. -/
def rewrite (c : Chat) (index : Index) (box : ByteArray) : Chat :=
  let (id, c) := c.fresh
  { c with jobs := c.jobs ++ [{ id, index := some index, box }] }

/-! ## Reading -/

def textOf (content : ByteArray) : String :=
  match Gcm.decode content with
  | some g =>
    if g.msgType = Gcm.MsgType.text then g.text.getD ""
    else if g.msgType = Gcm.MsgType.fileUpload then "[a file that cannot be read]"
    else "[a message that cannot be shown]"
  | none => "[a message that cannot be read]"

/-- A size as people read it. -/
def sizeText (n : Nat) : String :=
  if n < 1024 then s!"{n} bytes"
  else if n < 1024 * 1024 then s!"{(n + 512) / 1024} KB"
  else s!"{n / (1024 * 1024)}.{n % (1024 * 1024) * 10 / (1024 * 1024)} MB"

/-- How a line carrying a file reads, where it cannot be shown. -/
def fileText (a : Attachment) : String := s!"[file {a.name}, {sizeText a.size}]"

/-- The file under the state directory for line `id` of `c`, carrying a file called `name`. -/
def attachmentFile (c : Chat) (id now : Nat) (name : String) : String :=
  s!"attachments/{slug c.name}/{now}-{id}-{name}"

/-- A line for `content` from `sender`, and the file it carries, kept under the state
directory. -/
def deliver (c : Chat) (sender : Key) (index : Index) (content : ByteArray) : CoreM Chat := do
  let upload := (Gcm.decode content).bind Files.fileOf
  match upload with
  | some f =>
    let f := match (← getFlight).arriving.lookup (sender, index) with
      | some payload => { f with payload }
      | none => f
    let name := Files.safeName f.basename
    let a : Attachment := { name, filetype := f.filetype, size := f.payload.size,
                            file := attachmentFile c c.nextId (← get).1.now name }
    if f.payload.size > Files.maxFile then
      return (c.say fun id => { id, sender, index, text := s!"[file {name}, {sizeText a.size}, too large to keep]" }).2
    emit (.file c.name a.file (some f.payload))
    return (c.say fun id => { id, sender, index, text := fileText a, attachment := some a }).2
  | none => return (c.say fun id => { id, sender, index, text := textOf content }).2

/-- Keep the lines `keep` holds of, and delete the files the others carry. -/
def dropLines (c : Chat) (keep : Line → Bool) : CoreM Chat := do
  for l in c.log do
    if let some a := l.attachment then
      unless keep l do emit (.file c.name a.file none)
  return { c with log := c.log.filter keep }

/-- Do what the member asks. -/
def effects (c : Chat) (es : List Effect) : CoreM Chat := do
  let mut c := c
  for e in es do
    match e with
    | .writeBox index payload =>
      let box := match payload with
        | .bytes b => some b
        | .message m => Gcm.box m
      if let some box := box then c := rewrite c index box
    | .writeTombstone index => c := rewrite c index .empty
    | .readBox stream readCap index _ => emit (.scanRead c.name stream readCap index)
    | .deriveIndexes stream start count => emit (.derive c.name stream start count)
    | .walk stream start targets keep => emit (.walk c.name stream start targets keep)
    | .deliver sender index content => c ← deliver c sender index content
    | .expire sender index =>
      c ← dropLines c fun l => !(l.sender == sender && l.index == index)
    | .sendJoinReply .. => pure ()
  return c

/-- What a box holds, as a member reads it. -/
def messageOf (plaintext : ByteArray) : Option Message :=
  match Gcm.unframe plaintext with
  | .final cbor => Gcm.toMessage cbor
  | _ => none

/-- A member as a notice names them: `me`, or the name they chose, filtered, after `~`. -/
def label (p : Presentation) (c : Chat) (key : Key) : String :=
  if c.group.any (·.me == key) then "me"
  else "~" ++ p.name ((c.names.get? key).getD "?")

/-- Notes for the conversation about a box: who an `Introduction` introduced, or a message this
client cannot show. -/
def remark (p : Presentation) (c : Chat) (stream : Key) (plaintext : ByteArray) : Chat :=
  match Gcm.unframe plaintext with
  | .final cbor =>
    match (Gcm.toMessage cbor).bind (·.introduction) with
    | some i =>
      let c := { c with names := c.names.setDefault (keyOf i.readCap) i.name }
      if c.group.any (·.me == keyOf i.readCap) then c
      else c.notice s!"{label p c stream} added ~{p.name i.name}"
    | none => c
  | .continued _ => c.notice s!"{label p c stream} sent a message that cannot be read"
  | .indirection .. | .unknown => c

/-! ## Under way -/

def readingNow (chat : String) (stream : Key) : CoreM Bool :=
  return (← getFlight).reading.any fun (c, k) => c == chat && k == stream

def startReads (c : Chat) : CoreM Chat := do
  let some g := c.group | return c
  let mut c := c
  for r in g.readers.values do
    if (← readingNow c.name r.stream) || c.downloads.any (·.stream == r.stream) then continue
    let at_ := c.cursors.getD r.stream r.next
    c := { c with cursors := c.cursors.set r.stream at_ }
    modifyFlight fun f => { f with reading := (c.name, r.stream) :: f.reading }
    emit (.read c.name r.stream r.readCap at_)
  return c

def startWrite (c : Chat) : CoreM Chat := do
  if (← getFlight).writing.contains c.name || c.writeCap.size = 0 then return c
  match c.jobs with
  | [] => return c
  | j :: rest =>
    if j.upload.any fun u => u.sent < u.total then return c
    let index := j.index.getD c.nextWrite
    modifyFlight fun f => { f with writing := c.name :: f.writing }
    emit (.write c.name j.id c.writeCap index j.box)
    return { c with jobs := { j with index := some index } :: rest }

/-- Write the control message the member owes, without waiting for something to send: until a
joiner says where it was introduced, no one can count its stream. -/
def startControl (c : Chat) : CoreM Chat := do
  unless c.group.any (·.owesControl) do return c
  let (out, c) := onGroup c (Group.compose none) []
  enqueue c out

/-- Hand the new member its reply, once everything owed is written. -/
def startInduction (c : Chat) : CoreM Chat := do
  let some i := c.inducting | return c
  let some g := c.group | return c
  if i.payload.size = 0 || i.sealing || i.sealed.isSome || c.jobs.any (·.token.isSome) then
    return c
  let name ← meIn c
  let who := ReplyWho.ofJoinReply (g.reply c.nextWrite) fun key =>
    if key == g.me then some name else c.names.get? key
  emit (.seal c.name i.voucher i.payload (ReplyWho.encode who))
  return { c with inducting := some { i with sealing := true } }

def blobOf (chat : String) (job : Nat) : CoreM (Option ByteArray) :=
  return (← getFlight).blobs.lookup (chat, job)

/-- How many boxes of an upload are written at once. -/
def window : Nat := 8

/-- Write the next boxes of each upload, making its substream, reading its message back and
deriving their indexes first. -/
def startUploads (c : Chat) : CoreM Unit := do
  for j in c.jobs do
    let some u := j.upload | continue
    let f ← getFlight
    let key := (c.name, j.id)
    if u.sent ≥ u.total || f.uploading.contains key then continue
    if u.writeCap.size = 0 then
      modifyFlight fun f => { f with uploading := key :: f.uploading }
      emit (.substream c.name j.id)
      continue
    match ← blobOf c.name j.id with
    | some blob =>
      let want := min window (u.total - u.sent)
      let ahead := (f.ahead.lookup key).getD []
      if ahead.length < want then
        modifyFlight fun f => { f with uploading := key :: f.uploading }
        emit (.chunkIndexes c.name j.id u.next (min (2 * window) (u.total - u.sent) - 1))
        continue
      for (index, i) in (ahead.take want).zipIdx do
        let n := u.sent + i
        let f ← getFlight
        if f.chunksOut.contains (key, n) || f.landed.any fun l => l.1 == key && l.2.1 == n then continue
        modifyFlight fun f => { f with chunksOut := (key, n) :: f.chunksOut }
        emit (.writeChunk c.name j.id n u.writeCap index (Files.chunk blob u.boxSize n))
    | none =>
      unless f.loading.contains (c.name, j.id) do
        modifyFlight fun f => { f with loading := (c.name, j.id) :: f.loading }
        emit (.load c.name j.id u.file)

/-- Read the next box of each download. -/
def startDownloads (c : Chat) : CoreM Unit := do
  for d in c.downloads do
    if ← readingNow c.name d.stream then continue
    let at_ := (((← getFlight).fetched.lookup (c.name, d.stream)).map (·.1)).getD (startOf d.readCap)
    modifyFlight fun f => { f with reading := (c.name, d.stream) :: f.reading }
    emit (.readChunk c.name d.stream d.readCap at_)

/-- Start whatever can start. -/
def pump : CoreM Unit := do
  for c in (← get).1.state.chats do
    let c ← startControl c
    let c ← startWrite c
    let c ← startReads c
    let c ← startInduction c
    startUploads c
    startDownloads c
    setChat c

def withChat (name : String) (f : Chat → CoreM Chat) : CoreM Unit := do
  if let some c ← getChat name then setChat (← f c)

/-- Whether `name` was another group's until it was renamed, which says so; work started under
it may still come back, and has to reach that group. -/
def formerly (name : String) : CoreM Bool := do
  let some now := (← getFlight).renamed.lookup name | return false
  withChat now fun c => return c.notice s!"{name} was this group's name until now: give the new group another one"
  return true

def stopReading (chat : String) (stream : Key) : CoreM Unit :=
  modifyFlight fun f => { f with reading := f.reading.filter fun (c, k) => !(c == chat && k == stream) }

def missed (chat : String) (stream : Key) : CoreM Nat := do
  let n := ((← getFlight).misses.lookup (chat, stream)).getD 0
  modifyFlight fun f => { f with misses := ((chat, stream), n + 1) :: f.misses.filter (·.1 != (chat, stream)) }
  return n

def hit (chat : String) (stream : Key) : CoreM Unit :=
  modifyFlight fun f => { f with misses := f.misses.filter (·.1 != (chat, stream)) }

/-- How long to wait before reading a box that was not there, after `n` misses before. -/
def backoff (n : Nat) : Nat := min 60000 (2000 * 2 ^ (min n 5))

/-- Move a cursor that a scan has carried past it. -/
def followReader (c : Chat) (stream : Key) : Chat :=
  match c.group.bind (·.readers.get? stream), c.cursors.get? stream with
  | some r, some at_ => if counter at_ < counter r.next then { c with cursors := c.cursors.set stream r.next } else c
  | _, _ => c

def sanitize (name : String) : String :=
  let s := String.ofList (name.toList.filter fun ch => !(ch.toNat < 0x20 || (0x7f ≤ ch.toNat && ch.toNat ≤ 0x9f)))
  if s.isEmpty then "unnamed" else s

/-- A download is over: tell the member what came of it, in its place in the stream. -/
def finish (c : Chat) (d : Download) (blob : Option ByteArray) : CoreM Chat := do
  modifyFlight fun f => { f with fetched := f.fetched.filter (·.1 != (c.name, d.stream)) }
  let p := (← get).1.present
  let c := { c with downloads := c.downloads.filter (·.stream != d.stream) }
  let blob := blob.map fun b => match Files.takePayload b with
    | some (without, payload) => (without, some payload)
    | none => (b, none)
  if let some (_, some payload) := blob then
    modifyFlight fun f => { f with arriving := [((d.stream, d.index), payload)] }
  let message := blob.bind (Gcm.toMessage ·.1)
  let c := match blob, message with
    | some (b, _), some _ => remark p c d.stream (Gcm.frame b)
    | _, _ => c.notice s!"{label p c d.stream} sent a message that could not be fetched"
  let (es, c) := onGroup c (Group.observe d.stream d.index message d.next) []
  let c ← effects c es
  modifyFlight fun f => { f with arriving := [] }
  return c

def mapJob (c : Chat) (job : Nat) (f : Job → Job) : Chat :=
  { c with jobs := c.jobs.map fun j => if j.id == job then f j else j }

def readResult : ReadOutcome → ReadResult × Option Index
  | .data p next => (.data (messageOf p), some next)
  | .notFound => (.missing .notFound, none)
  | .tombstone next => (.missing .tombstone, some next)

/-! ## Messages -/

def handle : Msg → CoreM Unit
  | .create name own => do
    if ← formerly name then return
    if let some c ← getChat name then
      setChat (c.notice s!"you already have a group called {name}")
      return
    setChat { name, me := own }
    emit (.create name false)
  | .join name own => do
    if ← formerly name then return
    if let some c ← getChat name then
      setChat (c.notice s!"you already have a group called {name}: to join someone else's group, give it a name of its own")
      return
    setChat { name, me := own, joining := some .minting }
    emit (.create name true)
  | .created name joining kp => withChat name fun c => do
    let c := { c with writeCap := kp.writeCap, nextWrite := kp.first }
    if joining then
      emit (.mint name kp.writeCap (← meIn c))
      return c.notice "minting a voucher"
    return { c with group := some (Group.found kp.readCap [kp.readCap]) }.notice
      (← get).1.present.newGroup
  | .minted name m box1 => withChat name fun c => do
    let code := Base64.encode m.voucher
    emit (.awaitReply name m.secretKey m.readCap box1 c.writeCap)
    return { c with joining := some (.awaiting code m.secretKey m.readCap box1) }.notice
      ((← get).1.present.voucherMinted code name (voucherFile name))
  | .opened name o => withChat name fun c => do
    let joined : Option (ReplyWho.ReplyWho × Group) := do
      let who ← ReplyWho.decode o.whoReply
      let reply ← ReplyWho.toJoinReply who
      let own := readCapOf o.mutatedWriteCap
      if reply.members.any (·.any fun m => keyOf m.1 == keyOf own) then none
      some (who, Group.joined own reply)
    match joined with
    | none => return { c with joining := none }.notice "the reply to our voucher cannot be read"
    | some (who, g) =>
      let names := who.pleaseAdds.foldl (fun (d : Dict Key String) p => match p with
        | some p => d.set (keyOf p.readCap) (sanitize p.name)
        | none => d) c.names
      return { c with group := some g, writeCap := o.mutatedWriteCap,
                      nextWrite := startOfWriteCap o.mutatedWriteCap, names, joining := none,
                      cursors := {} }.notice "joined the group"
  | .induct name code => withChat name fun c => do
    if c.group.isNone then return c.notice "join or create the group first"
    if c.inducting.isSome then return c.notice "already adding a member"
    match Base64.decode code with
    | none => return c.notice "that is not a voucher code"
    | some voucher =>
      emit (.readVoucher name voucher)
      return { c with inducting := some { voucher } }.notice "reading the voucher"
  | .joinerPayload name _ vwc payload box1 => withChat name fun c => do
    let some i := c.inducting | return c
    let (out, c) := onGroup c Group.beginInduction []
    let c ← enqueue c out
    return { c with inducting := some { i with payload, voucherWriteCap := vwc, box1 } }
  | .sealed name s => withChat name fun c => do
    let some i := c.inducting | return c
    emit (.publishReply name i.voucherWriteCap i.box1 s)
    return { c with inducting := some { i with sealed := some s } }
  | .inducted name i => withChat name fun c => do
    let who := sanitize i.name
    let (o, c) := onGroup c (Group.introduceNow who i.mutatedReadCap) default
    let c ← if c.group.isSome then enqueue c [o] else pure c
    let c := (onGroup c (do if (← get).ledger.holding then Group.cancelInduction) ()).2
    let c := { c with inducting := none, names := c.names.set (keyOf i.mutatedReadCap) who }
    return c.notice s!"added ~{(← get).1.present.name who}: they now read everything said in this group"
  | .send name text => withChat name fun c => do
    let text := (← get).1.present.outgoing text
    let some g := c.group | return c.notice "this conversation has not been joined yet"
    let content := Gcm.textContent (membershipHash (memberCaps g)) text
    let (id, c) := c.say fun id => { id, sender := g.me, text, status := .pending }
    let (out, c) := onGroup c (Group.compose (some content)) []
    enqueue c out (some content) (some id)
  | .sendFile name file bytes => withChat name fun c => do
    let some g := c.group | return c.notice "this conversation has not been joined yet"
    if bytes.size > Files.maxFile then
      return c.notice s!"that file is too large to send: at most {sizeText Files.maxFile}"
    let safe := Files.safeName file
    let f : FileUpload := { payload := .empty, filetype := Files.filetypeOf safe, basename := safe }
    let content := Files.fileContent (membershipHash (memberCaps g)) f
    let a : Attachment := { name := safe, filetype := f.filetype, size := bytes.size,
                            file := attachmentFile c c.nextId (← get).1.now safe }
    emit (.file name a.file (some bytes))
    let (id, c) := c.say fun id => { id, sender := g.me, text := fileText a, status := .pending,
                                     attachment := some a }
    let (out, c) := onGroup c (Group.compose (some content)) []
    enqueue c out (some content) (some id) (some bytes)
  | .filed name file error => withChat name fun c =>
    return match error with
      | some e => c.notice s!"{file} could not be kept: {e}"
      | none => c
  | .loaded name job bytes => do
    modifyFlight fun f => { f with loading := f.loading.erase (name, job) }
    withChat name fun c => do
      let some j := c.jobs.find? (·.id == job) | return c
      match bytes with
      | some b =>
        modifyFlight fun f => { f with blobs := ((name, job), b) :: f.blobs.filter (·.1 != (name, job)) }
        return c
      | none =>
        let c := match j.token with
          | some t => (onGroup c (Group.abandoned t) ()).2
          | none => c
        return { c with jobs := c.jobs.filter (·.id != job) }.notice "a message could not be sent: what it held was lost"
  | .substream name job kp => do
    modifyFlight fun f => { f with uploading := f.uploading.erase (name, job) }
    withChat name fun c => return mapJob c job fun j =>
      { j with upload := j.upload.map ({ · with writeCap := kp.writeCap, next := kp.first }) }
  | .chunkIndexes name job start indexes => do
    modifyFlight fun f => { f with uploading := f.uploading.erase (name, job) }
    withChat name fun c => do
      if (c.jobs.find? (·.id == job)).any (·.upload.any (·.next == start)) then
        modifyFlight fun f => { f with
          ahead := ((name, job), start :: indexes) :: f.ahead.filter (·.1 != (name, job)) }
      return c
  | .wroteChunk name job n next => do
    let key := (name, job)
    modifyFlight fun f => { f with chunksOut := f.chunksOut.erase (key, n) }
    withChat name fun c => do
      let some j := c.jobs.find? (·.id == job) | return c
      let some u := j.upload | return c
      if n < u.sent then return c
      modifyFlight fun f => { f with landed := (key, (n, next)) :: f.landed }
      -- Count the boxes written in a row from the first not yet counted.
      let mut u := u
      let mut f ← getFlight
      while true do
        let some (_, (_, after)) := f.landed.find? fun l => l.1 == key && l.2.1 == u.sent | break
        f := { f with landed := f.landed.filter (· != (key, (u.sent, after))),
                      ahead := f.ahead.map fun (k, is) => if k == key then (k, is.drop 1) else (k, is) }
        u := { u with sent := u.sent + 1, next := after }
      modifyFlight fun _ => f
      if u.sent < u.total then return mapJob c job fun j => { j with upload := some u }
      modifyFlight fun f => { f with blobs := f.blobs.filter (·.1 != key), ahead := f.ahead.filter (·.1 != key),
                                     landed := f.landed.filter (·.1 != key) }
      emit (.file name u.file none)
      return mapJob c job fun j =>
        { j with upload := some u, box := Files.indirection u.total (readCapOf u.writeCap) }
  | .readChunk name stream index outcome => do
    stopReading name stream
    withChat name fun c => do
      let some d := c.downloads.find? (·.stream == stream) | return c
      let (at_, parts) := ((← getFlight).fetched.lookup (name, stream)).getD (startOf d.readCap, #[])
      unless at_ == index do return c
      let max := (← get).1.maxPlaintext
      match outcome with
      | .notFound =>
        modifyFlight fun f => { f with reading := (name, stream) :: f.reading }
        emit (.wake name stream (backoff (← missed name stream)))
        return c
      | .tombstone _ => finish c d none
      | .data box next =>
        hit name stream
        let parts := parts.push box
        let tag := if box.size = 0 then 0 else box[0]!.toNat
        if tag == 'F'.toNat then finish c d (Files.assemble parts)
        else if tag == 'C'.toNat && parts.size * max ≤ Files.maxFile + 2 * max
            && d.total.all (parts.size < ·) then
          modifyFlight fun f => { f with
            fetched := ((name, stream), (next, parts)) :: f.fetched.filter (·.1 != (name, stream)) }
          return c
        else finish c d none
  | .wrote name job index next => do
    modifyFlight fun f => { f with writing := f.writing.erase name }
    withChat name fun c => do
      let some j := c.jobs.find? (·.id == job) | return c
      let mut c := { c with jobs := c.jobs.filter (·.id != job) }
      if let some t := j.token then
        c := (onGroup c (Group.written t index (some (.bytes j.box))) ()).2
        c := { c with nextWrite := next }
      if let some l := j.line then
        c := { c with log := c.log.map fun x =>
          if x.id == l then { x with status := .sent, idx64 := some (counter index) } else x }
      return c
  | .read name stream index outcome => do
    stopReading name stream
    withChat name fun c => do
      unless c.cursors.get? stream == some index do return c
      match outcome with
      | .notFound =>
        modifyFlight fun f => { f with reading := (name, stream) :: f.reading }
        emit (.wake name stream (backoff (← missed name stream)))
        return c
      | .tombstone next =>
        hit name stream
        return { c with cursors := c.cursors.set stream next }
      | .data plaintext next =>
        hit name stream
        if let .indirection total readCap := Gcm.unframe plaintext then
          let max := (← get).1.maxPlaintext
          if (total.getD 0) * max ≤ Files.maxFile + 2 * max then
            return { c with downloads := c.downloads ++ [{ stream, index, next, readCap, total }],
                            cursors := c.cursors.set stream next }
          let c := c.notice s!"{label (← get).1.present c stream} sent a file too large to fetch"
          let (es, c) := onGroup c (Group.observe stream index none next) []
          let c ← effects c es
          return { c with cursors := c.cursors.set stream next }
        let c := remark (← get).1.present c stream plaintext
        let (es, c) := onGroup c (Group.observe stream index (messageOf plaintext) next) []
        let c ← effects c es
        return { c with cursors := c.cursors.set stream next }
  | .wake name stream => stopReading name stream
  | .scan name => withChat name fun c => do
    let some g := c.group | return c
    let mut c := c
    for stream in g.readers.keys do
      let (es, c') := onGroup c (Group.handle (.scan stream)) []
      c ← effects c' es
    return c.notice "scanning for missed messages"
  | .leave name => do
    if let some c ← getChat name then
      for f in c.files do emit (.file name f none)
    modify fun (core, as) =>
      ({ core with state := { core.state with chats := core.state.chats.filter (·.name != name) } }, as)
    modifyFlight fun f => { writing := f.writing.erase name,
                            reading := f.reading.filter (·.1 != name),
                            misses := f.misses.filter (·.1.1 != name),
                            uploading := f.uploading.filter (·.1 != name),
                            ahead := f.ahead.filter (·.1.1 != name),
                            chunksOut := f.chunksOut.filter (·.1.1 != name),
                            landed := f.landed.filter (·.1.1 != name),
                            loading := f.loading.filter (·.1 != name),
                            blobs := f.blobs.filter (·.1.1 != name),
                            fetched := f.fetched.filter (·.1.1 != name) }
  | .remove name who => withChat name fun c => do
    unless c.group.any (·.readers.contains who) do return c.notice "that member is no longer in this group"
    let label := label (← get).1.present c who
    let c := (onGroup c (Group.remove who) ()).2
    stopReading name who
    hit name who
    modifyFlight fun f => { f with fetched := f.fetched.filter (·.1 != (name, who)) }
    let c ← dropLines c (·.sender != who)
    return { c with cursors := c.cursors.erase who,
                    downloads := c.downloads.filter (·.stream != who) }.notice
      s!"stopped reading {label} and deleted what they sent; they can still read this group"
  | .clear name => withChat name fun c => dropLines c fun _ => false
  | .rename name to => do
    let some c ← getChat name | return
    if to.isEmpty || to == name then return
    if (← getChat to).isSome then
      setChat (c.notice s!"you already have a group called {to}")
      return
    if ((← getFlight).renamed.lookup to).any (· != name) then
      setChat (c.notice s!"another group was called {to} until now: give this one another name")
      return
    let p := (← get).1.present
    let c := { c with name := to }.notice s!"renamed from {p.name name}"
    modify fun (core, as) => ({ core with
      state := { core.state with chats := core.state.chats.map fun d => if d.name == name then c else d },
      flight := core.flight.rename name to }, as)
  | .scanRead name stream index outcome => withChat name fun c => do
    let (result, next) := readResult outcome
    let (es, c) := onGroup c (Group.handle (.boxRead stream index result next)) []
    let c ← effects c es
    return followReader c stream
  | .walked name stream target skipped steps => withChat name fun c => do
    let (es, c) := onGroup c (Group.handle (.walked stream target skipped steps)) []
    let c ← effects c es
    return followReader c stream
  | .derived name stream indexes => withChat name fun c => do
    let (es, c) := onGroup c (Group.handle (.indexesDerived stream indexes)) []
    effects c es
  | .epoch n => do
    for c in (← get).1.state.chats do
      let some g := c.group | continue
      unless g.epoch < n do continue
      let (es, c) := onGroup c (Group.handle (.epochChanged n)) []
      setChat (← effects c es)
  | .failed name what => withChat name fun c => do
    let c := (onGroup c (do if (← get).ledger.holding then Group.cancelInduction) ()).2
    let c := match c.joining with
      | some .minting => { c with joining := none }
      | _ => c
    return { c with inducting := none }.notice what

end Core

/-- Each line added since the chats were as `before` holds, stamped with `now`. -/
def stamp (before : State) (now : Nat) (s : State) : State :=
  { s with chats := s.chats.map fun c =>
      let from_ := ((before.chat? c.name).map (·.nextId)).getD 0
      { c with log := c.log.map fun l => if l.id ≥ from_ && l.time.isNone then { l with time := some now } else l } }

/-- One message: the new core, and the network work to start. -/
def step (c : Core) (m : Msg) : Core × List Action :=
  let before := c.state
  let m := m.mapChat c.flight.current
  let ((), (c, as)) := (do Core.handle m; Core.pump).run (c, #[])
  ({ c with state := stamp before c.now c.state }, as.toList)

/-- Begin again after a restart: whatever was under way is started afresh. An induction carries
on from where it was: the voucher is read, the reply sealed, or the reply kept when it was sealed
published again. -/
def resume (present : Presentation) (s : State) (maxPlaintext : Nat) (now : Nat := 0) :
    Core × List Action :=
  let go : CoreM Unit := do
    for c in s.chats do
      if c.writeCap.size = 0 then
        Core.emit (.create c.name c.joining.isSome)
        continue
      match c.joining with
      | some .minting => Core.emit (.mint c.name c.writeCap (← Core.meIn c))
      | some (.awaiting _ sk rc box1) => Core.emit (.awaitReply c.name sk rc box1 c.writeCap)
      | none => pure ()
      match c.inducting with
      | some { sealed := some s, voucherWriteCap, box1, .. } =>
        Core.emit (.publishReply c.name voucherWriteCap box1 s)
      | some i =>
        if i.payload.size = 0 then Core.emit (.readVoucher c.name i.voucher)
        else Core.setChat { c with inducting := some { i with sealing := false } }
      | none => pure ()
    Core.pump
  let ((), (c, as)) := go.run ({ state := s, maxPlaintext, now, present }, #[])
  ({ c with state := stamp s now c.state }, as.toList)

/-- Whether any member has acknowledged our line `l`. -/
def acknowledged (c : Chat) (l : Line) : Bool :=
  match c.group, l.idx64 with
  | some g, some n => g.ledger.ackedBy.values.any (n ≤ ·)
  | _, _ => false

end GroupChatClient.GroupChatKatzenqt1
