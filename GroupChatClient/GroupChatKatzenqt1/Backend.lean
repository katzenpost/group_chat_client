/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import GroupChatClient.Pigeonhole.Backend

/-! # What a member asks of the network

Pigeonhole's boxes, and the contact vouchers by which a member joins a group. -/

namespace GroupChatClient.GroupChatKatzenqt1

open GroupChatClient.Pigeonhole

structure Minted where
  voucher : ByteArray
  payload : ByteArray
  writeCap : ByteArray
  readCap : ByteArray
  secretKey : ByteArray

/-- Whether a voucher payload is the version 3 form, a CBOR map: answered with the map form of
the reply, unframed, in box 1 alone. Any other form is answered framed over boxes 1 on. -/
def mapForm (payload : ByteArray) : Bool := payload[0]?.any fun b => b.toNat / 32 == 5

structure Inducted where
  name : String
  mutatedReadCap : ByteArray
  sealedReply : ByteArray

structure Opened where
  whoReply : ByteArray
  mutatedWriteCap : ByteArray

/-- Each call throws on failure; a caller that must succeed retries. -/
structure Backend extends Pigeonhole.Backend where
  /-- `long`, the version 3 form of the payload, a CBOR map; else the compact form. -/
  voucherMint : (writeCap : ByteArray) → (name : String) → (long : Bool) → IO Minted
  /-- The voucher stream's write and read caps. -/
  voucherDeriveStream : (voucher : ByteArray) → IO (ByteArray × ByteArray)
  voucherInduct : (voucher payload whoReply : ByteArray) → IO Inducted
  voucherOpen : (secretKey sealedReply writeCap : ByteArray) → IO Opened

end GroupChatClient.GroupChatKatzenqt1
