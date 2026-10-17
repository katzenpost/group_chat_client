/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import GroupChatClient.GroupChatKatzenqt1.Persist.Group

/-! # What a member keeps

One `Chat` per conversation: our stream, the member state machine, where we have read each
member's stream, the boxes still to write, and the conversation as shown. It is saved whole, as
CBOR, after every change. -/

namespace GroupChatClient.GroupChatKatzenqt1

open Cryptlib.Protocols.GroupComm.Common
open Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member
open GroupChatClient.GroupChatKatzenqt1.Persist
open GroupChatClient.GroupChatKatzenqt1.Persist.Persist

/-- A message too long for a box, being written to a substream of its own. -/
structure Upload where
  /-- The file under the state directory that holds the message, until it is written. -/
  file : String
  /-- The most plaintext a box holds, which its boxes were cut to. -/
  boxSize : Nat
  total : Nat
  /-- The substream's, once made. -/
  writeCap : ByteArray := .empty
  /-- Boxes written so far, and the index of the next. -/
  sent : Nat := 0
  next : Index := .empty
  deriving Inhabited

/-- A box to write to our own stream, in order. -/
structure Job where
  id : Nat
  /-- The message's token, to tell the member when it is written; `none` for a rewrite. -/
  token : Option Nat := none
  /-- The box: chosen when the job is first sent, given for a rewrite. -/
  index : Option Index := none
  /-- What the box holds; empty for a tombstone. -/
  box : ByteArray
  /-- The conversation line it shows as. -/
  line : Option Nat := none
  /-- For a message too long for a box: its substream, which is written first; `box` is then the
  `I` box naming it. -/
  upload : Option Upload := none
  deriving Inhabited

inductive Status | pending | sent
  deriving DecidableEq, Inhabited

/-- A file a line carries. -/
structure Attachment where
  /-- Its name, made safe. -/
  name : String
  /-- As katzenqt tags it: `image/png`, `audio/opus`, `arbitrary`. -/
  filetype : String
  size : Nat
  /-- The file under the state directory that holds it. -/
  file : String
  deriving Inhabited, BEq

/-- A line of a conversation. -/
structure Line where
  id : Nat
  /-- Who wrote it; empty for a notice of our own. -/
  sender : Key := .empty
  /-- The box it came in, so that it can expire. -/
  index : Index := .empty
  text : String
  status : Status := .sent
  /-- The `Idx64` of our own message's box, once written. -/
  idx64 : Option Nat := none
  /-- When it was said here, or read: milliseconds since the Unix epoch, by this machine's clock;
  `none` for a line saved before times were kept. -/
  time : Option Nat := none
  attachment : Option Attachment := none
  deriving Inhabited

/-- Joining a group through a contact voucher we minted. -/
inductive Joining
  | minting
  /-- The voucher code to hand an existing member, and how to read its reply. -/
  | awaiting (code : String) (secretKey voucherReadCap box1 : ByteArray)
  deriving Inhabited

/-- Inducting a member whose voucher we were given. -/
structure Inducting where
  voucher : ByteArray
  payload : ByteArray
  voucherWriteCap : ByteArray
  box1 : Index
  name : String
  /-- The reply is on its way to box 1. -/
  sealing : Bool := false
  deriving Inhabited

/-- A message a member's `I` box announced, being read from its substream. Their stream is read
no further until it is. -/
structure Download where
  stream : Key
  /-- The `I` box, and the one after it. -/
  index : Index
  next : Index
  readCap : ByteArray
  /-- How many boxes the substream holds, when the writer said. -/
  total : Option Nat
  deriving Inhabited

structure Chat where
  name : String
  writeCap : ByteArray := .empty
  nextWrite : Index := .empty
  group : Option Group := none
  /-- Where we read each member's stream next. -/
  cursors : Dict Key Index := {}
  names : Dict Key String := {}
  jobs : List Job := []
  log : List Line := []
  joining : Option Joining := none
  inducting : Option Inducting := none
  nextId : Nat := 0
  downloads : List Download := []
  deriving Inhabited

structure State where
  /-- Our display name. -/
  me : String
  chats : List Chat := []
  deriving Inhabited

namespace Chat

def fresh (c : Chat) : Nat × Chat := (c.nextId, { c with nextId := c.nextId + 1 })

/-- Lines a conversation keeps; older ones are dropped. -/
def maxLog : Nat := 10000

/-- Add a line; its id. -/
def say (c : Chat) (line : Nat → Line) : Nat × Chat :=
  let (id, c) := c.fresh
  let log := c.log ++ [line id]
  (id, { c with log := log.drop (log.length - min log.length maxLog) })

def notice (c : Chat) (text : String) : Chat := (c.say fun id => { id, text }).2

end Chat

def slug (group : String) : String :=
  String.ofList (group.toList.map fun ch => if ch.isAlphanum then ch else '_')

/-- The file beside the state that holds the voucher code minted to join `group`. -/
def voucherFile (group : String) : String := s!"voucher-{slug group}.txt"

/-- The files a conversation's lines carry. -/
def Chat.files (c : Chat) : List String :=
  c.log.filterMap (·.attachment.map (·.file)) ++ c.jobs.filterMap (·.upload.map (·.file))

def State.chat? (s : State) (name : String) : Option Chat := s.chats.find? (·.name == name)

def State.setChat (s : State) (c : Chat) : State :=
  if s.chats.any (·.name == c.name) then
    { s with chats := s.chats.map fun d => if d.name == c.name then c else d }
  else { s with chats := s.chats ++ [c] }

/-! ## Saving -/

instance : Persist Upload where
  toTerm u := .TList #[toTerm u.file, toTerm u.boxSize, toTerm u.total, toTerm u.writeCap,
    toTerm u.sent, toTerm u.next]
  ofTerm t := do
    let fs ← fieldsOf t 6
    some ⟨← fieldAt fs 0, ← fieldAt fs 1, ← fieldAt fs 2, ← fieldAt fs 3, ← fieldAt fs 4,
      ← fieldAt fs 5⟩

/-- A job saved before uploads has five fields. -/
instance : Persist Job where
  toTerm j := .TList #[toTerm j.id, toTerm j.token, toTerm j.index, toTerm j.box, toTerm j.line,
    toTerm j.upload]
  ofTerm t := do
    let fs ← (fieldsOf t 6).orElse fun _ => fieldsOf t 5
    some ⟨← fieldAt fs 0, ← fieldAt fs 1, ← fieldAt fs 2, ← fieldAt fs 3, ← fieldAt fs 4,
      ← fieldAt fs 5⟩

instance : Persist Status where
  toTerm | .pending => toTerm (0 : Nat) | .sent => toTerm (1 : Nat)
  ofTerm t := do
    match (← ofTerm t : Nat) with
    | 0 => some .pending | 1 => some .sent | _ => none

instance : Persist Attachment where
  toTerm a := .TList #[toTerm a.name, toTerm a.filetype, toTerm a.size, toTerm a.file]
  ofTerm t := do
    let fs ← fieldsOf t 4
    some ⟨← fieldAt fs 0, ← fieldAt fs 1, ← fieldAt fs 2, ← fieldAt fs 3⟩

/-- A line is saved with its time and attachment; one saved before times were kept has six
fields, and one saved before attachments seven. -/
instance : Persist Line where
  toTerm l := .TList #[toTerm l.id, toTerm l.sender, toTerm l.index, toTerm l.text,
    toTerm l.status, toTerm l.idx64, toTerm l.time, toTerm l.attachment]
  ofTerm t := do
    let fs ← [8, 7, 6].firstM (fieldsOf t ·)
    some ⟨← fieldAt fs 0, ← fieldAt fs 1, ← fieldAt fs 2, ← fieldAt fs 3, ← fieldAt fs 4,
      ← fieldAt fs 5, ← fieldAt fs 6, ← fieldAt fs 7⟩

instance : Persist Joining where
  toTerm
    | .minting => .TList #[toTerm (0 : Nat)]
    | .awaiting code sk rc box1 => .TList #[toTerm (1 : Nat), toTerm code, toTerm sk, toTerm rc,
        toTerm box1]
  ofTerm t := do
    let ts ← termList? t
    match ts with
    | [k] => if (← ofTerm k : Nat) = 0 then some .minting else none
    | [_, code, sk, rc, box1] =>
      some (.awaiting (← ofTerm code) (← ofTerm sk) (← ofTerm rc) (← ofTerm box1))
    | _ => none

instance : Persist Inducting where
  toTerm i := .TList #[toTerm i.voucher, toTerm i.payload, toTerm i.voucherWriteCap,
    toTerm i.box1, toTerm i.name, toTerm i.sealing]
  ofTerm t := do
    let fs ← fieldsOf t 6
    some ⟨← fieldAt fs 0, ← fieldAt fs 1, ← fieldAt fs 2, ← fieldAt fs 3, ← fieldAt fs 4,
      ← fieldAt fs 5⟩

instance : Persist Download where
  toTerm d := .TList #[toTerm d.stream, toTerm d.index, toTerm d.next, toTerm d.readCap,
    toTerm d.total]
  ofTerm t := do
    let fs ← fieldsOf t 5
    some ⟨← fieldAt fs 0, ← fieldAt fs 1, ← fieldAt fs 2, ← fieldAt fs 3, ← fieldAt fs 4⟩

/-- A chat saved before downloads has eleven fields. -/
instance : Persist Chat where
  toTerm c := .TList #[toTerm c.name, toTerm c.writeCap, toTerm c.nextWrite, toTerm c.group,
    toTerm c.cursors, toTerm c.names, toTerm c.jobs, toTerm c.log, toTerm c.joining,
    toTerm c.inducting, toTerm c.nextId, toTerm c.downloads]
  ofTerm t := do
    let fs ← (fieldsOf t 12).orElse fun _ => fieldsOf t 11
    some ⟨← fieldAt fs 0, ← fieldAt fs 1, ← fieldAt fs 2, ← fieldAt fs 3, ← fieldAt fs 4,
      ← fieldAt fs 5, ← fieldAt fs 6, ← fieldAt fs 7, ← fieldAt fs 8, ← fieldAt fs 9,
      ← fieldAt fs 10, (fieldAt fs 11).getD []⟩

instance : Persist State where
  toTerm s := .TList #[toTerm s.me, toTerm s.chats]
  ofTerm t := do let fs ← fieldsOf t 2; some ⟨← fieldAt fs 0, ← fieldAt fs 1⟩

end GroupChatClient.GroupChatKatzenqt1
