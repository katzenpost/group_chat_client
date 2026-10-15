/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import Std.Data.HashMap
import GroupChatClient.Pigeonhole.Backend

/-! # An in-memory Pigeonhole

A `Backend` for tests. A stream is named by the first 32 bytes of its read cap; an index is its
`Idx64` (8 bytes, little-endian) followed by the stream's key and padding. Boxes are kept as
katzenpost's replicas keep them (`replica/state.go`, as katzenqt's `groupchat/sim.py` models it):
only those stored in the current or the previous replica epoch can be read; writing a box still
kept succeeds if it holds the same bytes, without storing it again, and fails otherwise; an empty
write is a tombstone, which is always stored and shadows what came before. The daemon's
cryptography is left out. -/

namespace GroupChatClient.Pigeonhole.Sim

/-- What a box holds: bytes, or a tombstone. -/
abbrev Box := Option ByteArray

structure World where
  /-- Each box, with the epoch it was stored in. -/
  boxes : Std.HashMap (List UInt8 × Nat) (Nat × Box) := {}
  epoch : Option Nat := some 1
  /-- Writes to fail before one succeeds, to test retrying. -/
  failWrites : Nat := 0
  maxPlaintext : Nat := 1530
  /-- Writes refused, each a box already holding other bytes: stream key and Idx64. -/
  refused : Array (List UInt8 × Nat) := #[]
  /-- Each box written, in order: stream key, Idx64 and bytes. -/
  writes : Array (List UInt8 × Nat × ByteArray) := #[]

def World.current (w : World) : Nat := w.epoch.getD 0

/-- What a box stored in epoch `e` holds now, if a replica still keeps it. -/
def World.kept (w : World) (key : List UInt8) (n : Nat) : Option Box := do
  let (e, box) ← w.boxes.get? (key, n)
  if e + 1 ≥ w.current then some box else none

def indexOf (key : ByteArray) (n : Nat) : Index :=
  let le := (List.range 8).map fun i => (n / 256 ^ i % 256).toUInt8
  ⟨(le ++ key.data.toList ++ List.replicate (indexSize - 8 - key.size) 0).toArray⟩

def keyOfIndex (i : Index) : List UInt8 := (i.extract 8 40).data.toList

def capsOf (key : ByteArray) : ByteArray × ByteArray :=
  let readCap := key ++ indexOf key 0
  (ByteArray.mk (List.replicate 32 (0 : UInt8)).toArray ++ readCap, readCap)

def randomKey : IO ByteArray := IO.getRandomBytes 32

def backend (world : IO.Ref World) : Backend where
  newKeypair := do
    let (wc, rc) := capsOf (← randomKey)
    pure { writeCap := wc, readCap := rc, first := startOf rc }
  writeBox wc index plaintext := do
    let w ← world.get
    if w.failWrites > 0 then
      world.set { w with failWrites := w.failWrites - 1 }
      throw (IO.userError "sim: write failed")
    let key := (keyOf (readCapOf wc)).data.toList
    unless keyOfIndex index == key do throw (IO.userError "sim: index of another stream")
    let n := counter index
    let box : Box := if plaintext.size = 0 then none else some plaintext
    let stored := { w with boxes := w.boxes.insert (key, n) (w.current, box),
                           writes := w.writes.push (key, n, plaintext) }
    match box, w.kept key n with
    | some new, some old =>
      if old.map (·.data) != some new.data then
        world.set { w with refused := w.refused.push (key, n) }
        throw (IO.userError "sim: box already exists")
      world.set { w with writes := w.writes.push (key, n, plaintext) }
    | _, _ => world.set stored
    pure (indexOf ⟨key.toArray⟩ (n + 1))
  readBox rc index := do
    let key := (keyOf rc).data.toList
    unless keyOfIndex index == key do throw (IO.userError "sim: index of another stream")
    let n := counter index
    let next := indexOf ⟨key.toArray⟩ (n + 1)
    match (← world.get).kept key n with
    | none => pure .notFound
    | some none => pure (.tombstone next)
    | some (some b) => pure (.data b next)
  nextIndex index := pure (indexOf (index.extract 8 40) (counter index + 1))
  replicaEpoch := return (← world.get).epoch
  maxPlaintext := return (← world.get).maxPlaintext

end GroupChatClient.Pigeonhole.Sim
