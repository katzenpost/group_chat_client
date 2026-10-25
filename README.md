# group_chat_client

The client engine that [jam](https://github.com/katzenpost/jam) and
[frank_chat](https://github.com/katzenpost/frank_chat) share: everything a group chat client does below its
user interface. It drives the member state machine of
[Cryptlib](https://github.com/katzenpost/cryptlib), talks to the client daemon over the
[thin client](https://github.com/katzenpost/thin_client), and keeps the client's state on disk.

## Layout

```
GroupChatClient/
  Pigeonhole/            boxes, whatever group protocol is spoken
    Backend.lean         what a client asks of Pigeonhole
    Daemon.lean          that, over kpclientd
    Box.lean             writing a box until it is written
    Sim.lean             an in-memory Pigeonhole for tests
  Shell.lean             clock, sleeping until stopped, private files written whole to the disk
  LogPane.lean           log lines kept for a pane
  Base64.lean
  GroupChatKatzenqt1/    the group chat protocol of that name in Cryptlib
    Backend.lean         Pigeonhole and contact vouchers
    Daemon.lean, Sim.lean
    Persist/             how this protocol's state is saved
      Codec.lean         each kind of thing kept, as CBOR
      Group.lean         the member state machine's group
    State.lean           what a member keeps
    Files.lean           files, and messages too long for a box
    Presentation.lean    how the app words and filters what the core says
    Core.lean            the pure state machine: step, resume
    Exec.lean            carrying out the core's actions
    Shell.lean           the event loop
    Tests/               unit tests and scenarios over the simulated Pigeonhole
  Tests/                 the test runner, and what the protocols' tests share
```

The client's state is saved by writing a temporary file, calling `fsync` on it, renaming it over
the old one and calling `fsync` on the directory, so that a crash or a power cut leaves the old
state or the new one, never part of either. Lean's `IO.FS.Handle.flush` only hands Lean's buffer
to the kernel, so `Shell.replacePrivate` calls `fsync(2)` through
[lean_os](https://github.com/katzenpost/lean_os), which has the operating system calls Lean's
standard library lacks. The package itself is all Lean.

Each group protocol has a directory of its own, named as in Cryptlib's
`Protocols/GroupComm/`. What is outside these directories never imports one, and none imports
another: a new protocol gets a new directory with its own core, state and saved format.

An app chooses a protocol by importing its directory, and supplies a `Presentation`, its display
and its controls.

## Building and testing

Building needs Lean, the version in `lean-toolchain`, from
[elan](https://github.com/leanprover/elan), and Rust, from [rustup](https://rustup.rs), for the
Rust inside lean_os and lean_unix_socket, which Lake builds with cargo.

```
make build
make test
```
