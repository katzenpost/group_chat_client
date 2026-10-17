/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member.Gcm

/-! # Files, and messages too long for a box

A file travels as katzenqt's `file_upload`: a map of its bytes, its type and its name. A message
too long for one box is written to a substream of its own, `C` chunks then an `F` chunk, and our
stream gets an `I` box: the number of chunks, four bytes big-endian, then the substream's read
cap. -/

namespace GroupChatClient.GroupChatKatzenqt1.Files

open Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member
open Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member.Cbor

structure FileUpload where
  payload : ByteArray
  /-- `image/png`, `audio/opus` or `arbitrary`, as katzenqt tags it. -/
  filetype : String
  basename : String

/-- The largest file sent or fetched. -/
def maxFile : Nat := 16 * 1024 * 1024

def encodeUpload (f : FileUpload) : List UInt8 :=
  header 5 3 ++ Cbor.text "payload" ++ bytes f.payload.data.toList
    ++ Cbor.text "filetype" ++ Cbor.text f.filetype ++ Cbor.text "basename" ++ Cbor.text f.basename

def uploadEntries : Nat → (Option ByteArray × Option String × Option String) → List UInt8 →
    Option (Option ByteArray × Option String × Option String)
  | 0, acc, _ => some acc
  | n + 1, (p, t, b), l => do
    let (k, l) ← readText l
    if k = "payload" then
      let (v, l) ← readBytes l
      uploadEntries n (some ⟨v.toArray⟩, t, b) l
    else if k = "filetype" then
      let (v, l) ← readText l
      uploadEntries n (p, some v, b) l
    else if k = "basename" then
      let (v, l) ← readText l
      uploadEntries n (p, t, some v) l
    else
      uploadEntries n (p, t, b) (← skipItems (l.length + 1) 1 l)

def decodeUpload (raw : List UInt8) : Option FileUpload := do
  let (major, n, rest) ← readHeader raw
  if major ≠ 5 then none
  let (p, t, b) ← uploadEntries n (none, none, none) rest
  some ⟨← p, ← t, ← b⟩

/-- The file a message carries. -/
def fileOf (g : Gcm.Gcm) : Option FileUpload := do
  if g.msgType ≠ Gcm.MsgType.fileUpload then none
  decodeUpload (← g.other.lookup "file_upload")

/-- A file, as a member's content. -/
def fileContent (membershipHash : ByteArray) (f : FileUpload) : ByteArray :=
  Gcm.encode { version := Gcm.currentVersion, membershipHash, msgType := Gcm.MsgType.fileUpload,
               other := [("file_upload", encodeUpload f)] }

/-- A name from elsewhere as a file may be called here: printable ASCII from katzenqt's list, no
leading dots, at most 200 characters. -/
def safeName (name : String) : String :=
  let ok (c : Char) := c.isAlphanum || " ._-+()[]".contains c
  let s := String.ofList ((name.toList.map fun c => if c.toNat < 128 && ok c then c else '_').dropWhile (· == '.'))
  if s.isEmpty then "unnamed" else String.ofList (s.toList.take 200)

def extension (name : String) : String :=
  match (name.splitOn ".").reverse with
  | ext :: _ :: _ => ext.toLower
  | _ => ""

/-- The type katzenqt tags a file with, from its name. -/
def filetypeOf (name : String) : String :=
  match extension name with
  | "png" => "image/png"
  | "jpg" | "jpeg" => "image/jpeg"
  | "gif" => "image/gif"
  | "webp" => "image/webp"
  | "bmp" => "image/bmp"
  | "opus" => "audio/opus"
  | _ => "arbitrary"

/-- Whether a file is shown as a picture, by its tag or else its name. -/
def isImage (filetype name : String) : Bool :=
  filetype.startsWith "image/" || (filetypeOf name).startsWith "image/"

/-! ## A file's bytes, kept out of the message

A message is read and written as a list of bytes, so a file's bytes stay out of it: the message
carries an empty payload, and the bytes are spliced in as it is written and out as it is read. -/

/-- An item's major type, its argument, and where what follows its header begins. -/
def headerAt (b : ByteArray) (pos : Nat) : Option (Nat × Nat × Nat) := do
  let first := (← b[pos]?).toNat
  let ai := first % 32
  if ai < 24 then return (first / 32, ai, pos + 1)
  if ai ≥ 28 then none
  let k := 2 ^ (ai - 24)
  if pos + 1 + k > b.size then none
  let n := (List.range k).foldl (fun n i => n * 256 + b[pos + 1 + i]!.toNat) 0
  return (first / 32, n, pos + 1 + k)

/-- Where the item at `pos` ends. -/
def skipAt (b : ByteArray) : Nat → Nat → Option Nat
  | 0, _ => none
  | fuel + 1, pos => do
    let (major, n, at_) ← headerAt b pos
    match major with
    | 0 | 1 | 7 => some at_
    | 2 | 3 => if at_ + n ≤ b.size then some (at_ + n) else none
    | 4 => (List.range n).foldlM (fun p _ => skipAt b fuel p) at_
    | 5 => (List.range (2 * n)).foldlM (fun p _ => skipAt b fuel p) at_
    | 6 => skipAt b fuel at_
    | _ => none

def textAt (b : ByteArray) (pos : Nat) : Option (String × Nat) := do
  let (major, n, at_) ← headerAt b pos
  if major ≠ 3 || at_ + n > b.size then none
  return (← String.fromUTF8? (b.extract at_ (at_ + n)), at_ + n)

/-- The entry of the map at `pos` whose key is `key`: where its value begins. -/
def lookupAt (b : ByteArray) (pos : Nat) (key : String) : Option Nat := do
  let (major, n, at_) ← headerAt b pos
  if major ≠ 5 then none
  let rec go : Nat → Nat → Option Nat
    | 0, _ => none
    | k + 1, p => do
      let (name, value) ← textAt b p
      if name = key then some value else go k (← skipAt b (b.size + 1) value)
  go n at_

/-- Where the file's bytes are in a message: the payload item's start and end. -/
def payloadSpan (cbor : ByteArray) : Option (Nat × Nat) := do
  let upload ← lookupAt cbor 0 "file_upload"
  let start ← lookupAt cbor upload "payload"
  let (major, _, _) ← headerAt cbor start
  if major ≠ 2 then none
  return (start, ← skipAt cbor 1 start)

/-- A message with the file's bytes taken out, and the bytes. -/
def takePayload (cbor : ByteArray) : Option (ByteArray × ByteArray) := do
  let (start, stop) ← payloadSpan cbor
  let (_, _, data) ← headerAt cbor start
  return (cbor.extract 0 start ++ ByteArray.mk #[0x40] ++ cbor.extract stop cbor.size,
    cbor.extract data stop)

/-- A message with `payload` put in place of the file's bytes. -/
def putPayload (cbor payload : ByteArray) : Option ByteArray := do
  let (start, stop) ← payloadSpan cbor
  return cbor.extract 0 start ++ ByteArray.mk (header 2 payload.size).toArray ++ payload
    ++ cbor.extract stop cbor.size

/-! ## Substreams -/

/-- How many boxes of `max` bytes a message of `n` bytes takes in a substream. -/
def chunkCount (n max : Nat) : Nat := (n + (max - 2)) / (max - 1)

/-- The `i`th box of `blob`'s substream. -/
def chunk (blob : ByteArray) (max i : Nat) : ByteArray :=
  let size := max - 1
  let tag := if i + 1 < chunkCount blob.size max then 'C' else 'F'
  ByteArray.mk #[tag.toNat.toUInt8] ++ blob.extract (i * size) ((i + 1) * size)

/-- The `I` box naming a substream of `total` boxes. -/
def indirection (total : Nat) (readCap : ByteArray) : ByteArray :=
  ByteArray.mk (#['I'.toNat.toUInt8] ++ (be 4 total).toArray) ++ readCap

/-- The message a substream's boxes hold, once its `F` box is among them. -/
def assemble (boxes : Array ByteArray) : Option ByteArray := do
  let last ← boxes.back?
  if last.size = 0 || last[0]! ≠ 'F'.toNat.toUInt8 then none
  let tags := boxes.pop.all fun b => b.size > 0 && b[0]! = 'C'.toNat.toUInt8
  if !tags then none
  some (boxes.foldl (fun acc b => acc ++ b.extract 1 b.size) .empty)

end GroupChatClient.GroupChatKatzenqt1.Files
