/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import GroupChatClient.GroupChatKatzenqt1.Tests.Core

/-! # Files and long messages

Their encoding, their substreams, and members sending them to each other over the simulated
Pigeonhole. -/

namespace GroupChatClient.GroupChatKatzenqt1.Tests.FilesTests

open GroupChatClient GroupChatClient.Tests GroupChatClient.GroupChatKatzenqt1.Tests.CoreTests
open Cryptlib.Protocols.GroupComm.Common
open Cryptlib.Protocols.GroupComm.GroupChatKatzenqt1.Member
open GroupChatClient.GroupChatKatzenqt1.Persist
open GroupChatClient.GroupChatKatzenqt1.Persist.Persist

def bytesOf (n : Nat) (seed : Nat := 7) : ByteArray :=
  ⟨(List.range n).toArray.map fun i => ((i * 31 + seed) % 251).toUInt8⟩

def fileOf? (n : Net) (who : Nat) (name : String) : Option ByteArray :=
  (((n.files.lookup who).getD []).lookup name : Option ByteArray)

def attachmentOf (n : Net) (who : Nat) (group name : String) : Option Attachment :=
  (chatOf n who group).bind fun c => c.log.findSome? fun l => l.attachment.filter (·.name == name)

/-- Whether `who` has the file a line of theirs called `name` carries. -/
def received (n : Net) (who : Nat) (group name : String) : Bool :=
  (attachmentOf n who group name).any fun a => (fileOf? n who a.file).isSome

def encoding : TestM Unit := do
  let max := 100
  for n in [1, 98, 99, 100, 198, 199, 1000] do
    let blob := bytesOf n
    let count := Files.chunkCount n max
    let boxes := (List.range count).toArray.map (Files.chunk blob max)
    check s!"{n} bytes: every box fits" (boxes.all (·.size ≤ max))
    check s!"{n} bytes: boxes are C then F" (boxes.toList.zipIdx.all fun (b, i) =>
      b[0]! == (if i + 1 < count then 'C' else 'F').toNat.toUInt8)
    check s!"{n} bytes: the boxes reassemble" ((Files.assemble boxes).map (·.data) == some blob.data)
    check s!"{n} bytes: without its last box, nothing" (count < 2 || (Files.assemble boxes.pop).isNone)
  let cap := bytesOf 136
  let i := Files.indirection 7 cap
  checkEq "an I box is 141 bytes" 141 i.size
  check "an I box reads back" (match Gcm.unframe i with
    | .indirection (some 7) c => c.data == cap.data
    | _ => false)
  let f : Files.FileUpload := { payload := bytesOf 3000, filetype := "image/png", basename := "cat.png" }
  let content := Files.fileContent (bytesOf 32) f
  let back := (Gcm.decode content).bind Files.fileOf
  check "a file reads back from its message" (back.any fun b =>
    b.payload.data == f.payload.data && b.filetype == "image/png" && b.basename == "cat.png")
  let (without, payload) := (Files.takePayload content).getD (.empty, .empty)
  check "a file's bytes come out of its message" (payload.data == f.payload.data && without.size < 200)
  check "and go back in" ((Files.putPayload without payload).map (·.data) == some content.data)
  check "a message without a file has no bytes to take" (Files.takePayload (Gcm.textContent (bytesOf 32) "hi")).isNone
  checkEq "a name is made safe" "_.._etc_passwd" (Files.safeName "../../etc/passwd")
  checkEq "an empty name" "unnamed" (Files.safeName "")
  checkEq "a picture's type" "image/jpeg" (Files.filetypeOf "Holiday.JPG")
  checkEq "anything else" "arbitrary" (Files.filetypeOf "notes.txt")

def saving : TestM Unit := do
  let a : Attachment := { name := "cat.png", filetype := "image/png", size := 3, file := "attachments/g/1-0-cat.png" }
  let l : Line := { id := 1, text := "[file cat.png, 3 bytes]", attachment := some a }
  check "a line saves with its file" ((ofTerm (toTerm l) : Option Line).any (·.attachment == some a))
  let old := match toTerm l with
    | .TList fs => Cborg.Term.TList (fs.extract 0 7)
    | t => t
  check "a line saved before files loads, with none" ((ofTerm old : Option Line).any (·.attachment.isNone))
  let j : Job := { id := 3, box := .empty, upload := some { file := "spool/x", boxSize := 1530, total := 9, sent := 4 } }
  check "an upload saves" ((ofTerm (toTerm j) : Option Job).any fun j => j.upload.any (·.sent == 4))
  let d : Download := { stream := bytesOf 32, index := bytesOf 104, next := bytesOf 104 1, readCap := bytesOf 136, total := some 9 }
  let c : Chat := { name := "g", downloads := [d] }
  check "a download saves" ((ofTerm (toTerm c) : Option Chat).any (·.downloads.length == 1))
  let old := match toTerm c with
    | .TList fs => Cborg.Term.TList (fs.extract 0 11)
    | t => t
  check "a chat saved before downloads loads" ((ofTerm old : Option Chat).any (·.downloads.isEmpty))

def longMessage : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let text := String.ofList ((List.range 5000).map fun i => Char.ofNat (97 + i % 26))
  let (ok, _) ← (do
    unless ← foundAndJoin world do return (false, false)
    feed 0 (.send "g" text)
    feed 0 (.send "g" "after it")
    let read ← runUntil world fun n => says n 1 "g" text && says n 1 "g" "after it"
    let n : Net ← get
    let inOrder := (chatOf n 1 "g").any fun (c : Chat) =>
      ((c.log.map fun (l : Line) => l.text).reverse.take 2) == ["after it", text]
    return (read, inOrder)).run net
  check "a message too long for a box goes by a substream" ok.1
  check "and what was sent after it comes after it" ok.2

def image : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let pic := bytesOf 20000
  let (ok, _) ← (do
    unless ← foundAndJoin world do return (false, false, false, false, false, false)
    feed 0 (.sendFile "g" "cat.png" pic)
    let n : Net ← get
    let pending := (chatOf n 0 "g").any fun (c : Chat) => c.log.any fun l => l.attachment.isSome && l.status == Status.pending
    let read ← runUntil world fun n => received n 1 "g" "cat.png"
    discard <| runUntil world fun n => (chatOf n 0 "g").any fun (c : Chat) => c.jobs.isEmpty
    let n : Net ← get
    let kept := match attachmentOf n 1 "g" "cat.png" with
      | some a => a.filetype == "image/png" && a.size == 20000 && (fileOf? n 1 a.file).map ByteArray.data == some pic.data
      | none => false
    let mine := match attachmentOf n 0 "g" "cat.png" with
      | some a => (fileOf? n 0 a.file).map ByteArray.data == some pic.data
      | none => false
    let sent := (chatOf n 0 "g").any fun (c : Chat) => c.log.any fun l => l.attachment.isSome && l.status == Status.sent
    let spooled := ((n.files.lookup 0).getD []).any (·.1.startsWith "spool/")
    return (pending, read, kept, mine, sent, !spooled)).run net
  let (pending, read, kept, mine, sent, unspooled) := ok
  check "a file sent shows at once" pending
  check "bob receives alice's picture" read
  check "with its type, size and bytes" kept
  check "alice keeps what she sent" mine
  check "and sees it written" sent
  check "what was spooled to send it is gone" unspooled

def chunksOut (n : Net) : Nat :=
  (n.queue.filter fun (_, a) => match a with | .writeChunk .. => true | _ => false).size

def concurrent : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let doc := bytesOf 30000 5
  let (ok, _) ← (do
    unless ← foundAndJoin world do return (false, false)
    feed 0 (.sendFile "g" "big.bin" doc)
    let several ← runUntil world fun n => chunksOut n ≥ Core.window
    -- The replicas answer the writes in the opposite order.
    modify fun (n : Net) => { n with queue := n.queue.reverse }
    let read ← runUntil world fun n => received n 1 "g" "big.bin"
    let n : Net ← get
    let same := (attachmentOf n 1 "g" "big.bin").any fun (a : Attachment) => (fileOf? n 1 a.file).map ByteArray.data == some doc.data
    return (several, read && same)).run net
  check "an upload writes several boxes at once" ok.1
  check "and they may be written in any order" ok.2

def restartMidUpload : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let doc := bytesOf 40000 3
  let (ok, net) ← (do
    unless ← foundAndJoin world do return false
    feed 0 (.sendFile "g" "notes.txt" doc)
    runUntil world fun n => (chatOf n 0 "g").any fun (c : Chat) =>
      c.jobs.any fun (j : Job) => j.upload.any fun (u : Upload) => u.sent ≥ 5).run net
  check "an upload is under way" ok
  let some s := (load (save net.cores[0]!.state) : Option State) | check "alice's state loads" false
  let (core, actions) := resume .plain s 1530
  let net := { net with cores := net.cores.set! 0 core,
                        queue := net.queue.filter (·.1 != 0) ++ actions.toArray.map (0, ·),
                        later := net.later.filter (·.2.1 != 0) }
  let (ok, _) ← (do
    let read ← runUntil world fun n => received n 1 "g" "notes.txt"
    let n : Net ← get
    let same := match attachmentOf n 1 "g" "notes.txt" with
      | some a => a.filetype == "arbitrary" && (fileOf? n 1 a.file).map ByteArray.data == some doc.data
      | none => false
    return read && same).run net
  check "after a restart the upload carries on, and bob receives the file whole" ok

def limits : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let (ok, _) ← (do
    feed 0 (.create "g")
    discard <| runUntil world fun n => joined n 0 "g"
    feed 0 (.sendFile "g" "huge.bin" (ByteArray.mk (Array.replicate (Files.maxFile + 1) 0)))
    let n : Net ← get
    return (chatOf n 0 "g").any fun (c : Chat) =>
      c.jobs.isEmpty && c.log.any (·.text.startsWith "that file is too large to send")).run net
  check "a file too large is refused" ok

def removeDeletes : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let (ok, _) ← (do
    unless ← foundAndJoin world do return (false, false)
    feed 0 (.sendFile "g" "a.png" (bytesOf 500))
    unless ← runUntil world fun n => received n 1 "g" "a.png" do return (false, false)
    let some a := attachmentOf (← get) 1 "g" "a.png" | return (false, false)
    let alice : Key := (((chatOf (← get) 1 "g").bind (·.group)).map fun (g : Group) => g.readers.keys).getD [] |>.headD ByteArray.empty
    feed 1 (.remove "g" alice)
    discard <| runUntil world fun _ => false
    let removed := (fileOf? (← get) 1 a.file).isNone
    feed 0 (.leave "g")
    discard <| runUntil world fun _ => false
    let left := ((← get).files.lookup 0).all fun (fs : List (String × ByteArray)) => fs.isEmpty
    return (removed, left)).run net
  check "removing a member deletes the files they sent" ok.1
  check "leaving a group deletes its files" ok.2

/-- No saved state names a file not yet on disk: a crash just after saving must not leave a line
whose file is missing. -/
def writtenBeforeSaved : TestM Unit := do
  let (world, net) ← newNet 2 ["alice", "bob"]
  let pic := bytesOf 20000
  let note := bytesOf 300 3
  let (ok, n) ← (do
    unless ← foundAndJoin world do return false
    feed 0 (.sendFile "g" "cat.png" pic)
    feed 0 (.sendFile "g" "note.txt" note)
    let a ← runUntil world fun n => received n 1 "g" "cat.png" && received n 1 "g" "note.txt"
    let b ← runUntil world fun n => (chatOf n 0 "g").any fun (c : Chat) => c.jobs.isEmpty
    return a && b).run net
  check "bob receives both files" ok
  check "every file a saved state named was already there" n.unwritten.isEmpty
    fun _ => s!"{n.unwritten}"

def tests : TestM Unit := do
  encoding
  saving
  longMessage
  image
  concurrent
  restartMidUpload
  limits
  removeDeletes
  writtenBeforeSaved

end GroupChatClient.GroupChatKatzenqt1.Tests.FilesTests
