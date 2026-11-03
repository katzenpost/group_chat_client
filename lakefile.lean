import Lake
open Lake DSL System

package group_chat_client where
  version := v!"0.2.0"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require Logger from git "https://github.com/katzenpost/Logger.git" @ "51313822f6db9ff091f3b2a3e6298e1066ebea5d"

require thinclient from git "https://github.com/katzenpost/thin_client.git" @ "98ff985ad348156eccbe7d3f4c4849a47041f2d3"

require lean_unix_socket from git "https://github.com/katzenpost/lean_unix_socket.git" @ "v0.1.0"

require lean_os from git "https://github.com/katzenpost/lean_os.git" @ "v0.1.0"

-- Last, so that its pin wins over the older ones its dependents carry.
require cryptlib from git "https://github.com/katzenpost/cryptlib.git" @ "6d8c31e81fc187f24c80257f5a96689d3ba0fc5e"

@[default_target]
lean_lib GroupChatClient where
  globs := #[.andSubmodules `GroupChatClient]

lean_exe «group-chat-client-test» where
  root := `GroupChatClient.Tests.Main
