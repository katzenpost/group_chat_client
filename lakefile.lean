import Lake
open Lake DSL System

package group_chat_client where
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`relaxedAutoImplicit, false⟩]

require Logger from git "https://github.com/katzenpost/Logger.git" @ "61f4fc02ffa00401d59d98f25feda499f26b10e2"

require thinclient from git "https://github.com/katzenpost/thin_client.git" @ "cbc2272c156bbe67cbc6b996677811baff3eb404"

require lean_unix_socket from git "https://github.com/katzenpost/lean_unix_socket.git" @ "4bbfd6bb9df6bc9ce0d31252afff86d48683c9f2"

-- Last, so that its pin wins over the older ones its dependents carry.
require cryptlib from git "https://github.com/katzenpost/cryptlib.git" @ "6f651738d26db5ac18dd5670bf501cfc63723175"

@[default_target]
lean_lib GroupChatClient where
  globs := #[.andSubmodules `GroupChatClient]

lean_exe «group-chat-client-test» where
  root := `GroupChatClient.Tests.Main

/-- `fsync`, which Lean's standard library does not offer. -/
target fsync.o pkg : FilePath := do
  let oFile := pkg.buildDir / "c" / "fsync.o"
  let srcJob ← inputTextFile <| pkg.dir / "c" / "fsync.c"
  buildO oFile srcJob #["-I", (← getLeanIncludeDir).toString, "-O2"] #["-fPIC"] "cc" getLeanTrace

extern_lib group_chat_client_ffi pkg := do
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "group_chat_client_ffi") #[← fsync.o.fetch]
