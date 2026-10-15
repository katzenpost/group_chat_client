/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

/-! # Boxes on the network

What a client asks of Pigeonhole, whatever group protocol it speaks: the client daemon over the
thin client, or the in-memory Pigeonhole of the tests (`GroupChatClient.Pigeonhole.Sim`). Caps and
indexes are opaque bytes: a write cap is 168 bytes, a read cap 136 (the write cap's last 136), an
index 104. -/

namespace GroupChatClient.Pigeonhole

/-- A box: its 104-byte `MessageBoxIndex`. -/
abbrev Index := ByteArray

def keySize : Nat := 32
def indexSize : Nat := 104

/-- The public key a read cap begins with. -/
def keyOf (readCap : ByteArray) : ByteArray := readCap.extract 0 keySize

/-- The index a read cap starts at. -/
def startOf (readCap : ByteArray) : Index := readCap.extract keySize readCap.size

/-- The BACAP `Idx64` of `index`: its first 8 bytes, little-endian. -/
def counter (index : Index) : Nat :=
  (index.extract 0 8).toList.foldr (fun b n => b.toNat + 256 * n) 0

/-- The index a write cap starts at. -/
def startOfWriteCap (writeCap : ByteArray) : Index :=
  writeCap.extract (writeCap.size - indexSize) writeCap.size

/-- The read cap of the stream a write cap writes. -/
def readCapOf (writeCap : ByteArray) : ByteArray := writeCap.extract 32 writeCap.size

/-- What reading a box found. -/
inductive ReadOutcome
  | data (plaintext : ByteArray) (next : Index)
  | notFound
  | tombstone (next : Index)
  deriving Inhabited

structure Keypair where
  writeCap : ByteArray
  readCap : ByteArray
  first : Index

/-- Each call throws on failure; a caller that must succeed retries. -/
structure Backend where
  newKeypair : IO Keypair
  /-- Write `plaintext`, empty for a tombstone, to the box at `index`; the index after it. -/
  writeBox : (writeCap : ByteArray) → (index : Index) → (plaintext : ByteArray) → IO Index
  readBox : (readCap : ByteArray) → (index : Index) → IO ReadOutcome
  nextIndex : Index → IO Index
  /-- The replica epoch the storage replicas are in, once known. -/
  replicaEpoch : IO (Option Nat)
  /-- The most plaintext a box holds. -/
  maxPlaintext : IO Nat

end GroupChatClient.Pigeonhole
