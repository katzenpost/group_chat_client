/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import GroupChatClient.Pigeonhole.Sim
import GroupChatClient.GroupChatKatzenqt1.Backend

/-! # Vouchers over the in-memory Pigeonhole

A voucher's stream is named by the voucher itself, and the daemon's cryptography is left out: the
sealed reply is the reply, and nothing is salted. -/

namespace GroupChatClient.GroupChatKatzenqt1.Sim

open GroupChatClient.Pigeonhole GroupChatClient.Pigeonhole.Sim

export GroupChatClient.Pigeonhole.Sim (World)

def backend (world : IO.Ref World) : Backend :=
  { Pigeonhole.Sim.backend world with
    voucherMint := fun wc name => do
      let voucher ← randomKey
      let (vwc, vrc) := capsOf voucher
      let payload := name.toUTF8.push 0 ++ readCapOf wc
      pure { voucher, payload, writeCap := vwc, readCap := vrc, secretKey := voucher }
    voucherDeriveStream := fun voucher => pure (capsOf voucher)
    voucherInduct := fun _ payload whoReply => do
      let some nul := payload.data.toList.idxOf? 0 | throw (IO.userError "sim: bad payload")
      let name := String.fromUTF8! (payload.extract 0 nul)
      pure { name, mutatedReadCap := payload.extract (nul + 1) payload.size,
             sealedReply := whoReply }
    voucherOpen := fun _ sealed wc => pure { whoReply := sealed, mutatedWriteCap := wc } }

end GroupChatClient.GroupChatKatzenqt1.Sim
