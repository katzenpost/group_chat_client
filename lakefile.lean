import Lake
open Lake DSL System

package group_chat_client where
  version := v!"0.2.0"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require Logger from git "https://github.com/katzenpost/Logger.git" @ "51313822f6db9ff091f3b2a3e6298e1066ebea5d"

require thinclient from git "https://github.com/katzenpost/thin_client.git" @ "8a474dee211cea6cbc7539e5793e4aeef13d7eb5"

require lean_unix_socket from git "https://github.com/katzenpost/lean_unix_socket.git" @ "4928d11994868397da22399e14098a99449f4544"

require lean_os from git "https://github.com/katzenpost/lean_os.git" @ "20644b504bcc91d26b52cefd99a0de05155fb446"

-- Last, so that its pin wins over the older ones its dependents carry.
require cryptlib from git "https://github.com/katzenpost/cryptlib.git" @ "v0.2.0"

@[default_target]
lean_lib GroupChatClient where
  globs := #[.andSubmodules `GroupChatClient]

lean_exe «group-chat-client-test» where
  root := `GroupChatClient.Tests.Main
