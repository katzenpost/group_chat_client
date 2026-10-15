/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import GroupChatClient.Pigeonhole.Daemon
import GroupChatClient.GroupChatKatzenqt1.Backend

/-! # The client daemon

Pigeonhole's `Backend` over the thin client, with kpclientd's contact vouchers. -/

namespace GroupChatClient.GroupChatKatzenqt1.Daemon

open ThinClient
open GroupChatClient.Pigeonhole.Daemon

def backend (client : Client) : Backend :=
  { Pigeonhole.Daemon.backend client with
    voucherMint := fun wc name => runIO client do
      let m ← voucherMint wc name
      pure { voucher := m.voucher, payload := m.voucherPayload, writeCap := m.voucherWriteCap,
             readCap := m.voucherReadCap, secretKey := m.voucherSecretKey }
    voucherDeriveStream := fun voucher => runIO client do
      let d ← voucherDeriveStream voucher
      pure (d.voucherWriteCap, d.voucherReadCap)
    voucherInduct := fun voucher payload who => runIO client do
      let i ← voucherInduct voucher payload who
      pure { name := i.displayName, mutatedReadCap := i.mutatedMessageReadCap,
             sealedReply := i.sealedReply }
    voucherOpen := fun sk sealed wc => runIO client do
      let o ← voucherOpen sk sealed wc
      pure { whoReply := o.whoReply, mutatedWriteCap := o.mutatedMessageWriteCap } }

end GroupChatClient.GroupChatKatzenqt1.Daemon
