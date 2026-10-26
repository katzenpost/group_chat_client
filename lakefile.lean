import Lake
open Lake DSL System

package group_chat_client where
  version := v!"0.2.0"
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require Logger from git "https://github.com/katzenpost/Logger.git" @ "51313822f6db9ff091f3b2a3e6298e1066ebea5d"

require thinclient from git "https://github.com/katzenpost/thin_client.git" @ "9b5d47a9e92a4a3a50cc1fff29ab5ab9194f75c1"

require lean_unix_socket from git "https://github.com/katzenpost/lean_unix_socket.git" @ "2d27248dd646a49653de201049b75f8601bb0da1"

require lean_os from git "https://github.com/katzenpost/lean_os.git" @ "6d33aa0668aef431c8135ed794ea915a629df0c5"

-- Last, so that its pin wins over the older ones its dependents carry.
require cryptlib from git "https://github.com/katzenpost/cryptlib.git" @ "v0.2.0"

@[default_target]
lean_lib GroupChatClient where
  globs := #[.andSubmodules `GroupChatClient]

lean_exe «group-chat-client-test» where
  root := `GroupChatClient.Tests.Main
