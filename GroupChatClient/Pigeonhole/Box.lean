/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

import GroupChatClient.Pigeonhole.Backend

/-! # Writing a box until it is written -/

namespace GroupChatClient.Pigeonhole

/-- How long to wait before writing again after a failure, or reading a box again. -/
def retryMs : Nat := 5000

/-- Write a box, trying again until it is written. -/
partial def publish (b : Backend) (writeCap : ByteArray) (index : Index) (box : ByteArray) :
    IO Index := do
  try b.writeBox writeCap index box
  catch _ =>
    IO.sleep retryMs.toUInt32
    publish b writeCap index box

end GroupChatClient.Pigeonhole
