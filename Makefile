# SPDX-License-Identifier: AGPL-3.0-only

export PATH := $(HOME)/.cargo/bin:$(HOME)/.elan/bin:$(PATH)

.PHONY: build test

build:
	lake exe cache get && lake build

# In one process, with no daemon.
test:
	lake exe cache get && lake build group-chat-client-test && lake exe group-chat-client-test
