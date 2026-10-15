/-
SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
SPDX-License-Identifier: AGPL-3.0-only
-/

/-! # How the app words and filters what the core says

The core writes notices into the conversation, naming members and the app's own controls. Each app
decides how a member's name may reach the screen, what typed text is sent, and how its controls
are named. -/

namespace GroupChatClient.GroupChatKatzenqt1

structure Presentation where
  /-- A member's name as a notice shows it. -/
  name : String → String
  /-- Text the user typed, as it is sent. -/
  outgoing : String → String
  /-- The notice for a group just created. -/
  newGroup : String
  /-- The notice for a voucher code just minted to join group `chat`, its file beside the state
  being `file`. -/
  voucherMinted : (code chat file : String) → String

/-- Names and text as they are; for tests. -/
def Presentation.plain : Presentation where
  name := id
  outgoing := id
  newGroup := "a new group: adding a member's voucher code adds them"
  voucherMinted _ _ file := s!"give the voucher code to a member of the group (it is also in {file})"

instance : Inhabited Presentation := ⟨.plain⟩

end GroupChatClient.GroupChatKatzenqt1
