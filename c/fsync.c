// SPDX-FileCopyrightText: Copyright (C) 2026 David Stainton
// SPDX-License-Identifier: AGPL-3.0-only

#include <lean/lean.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>

LEAN_EXPORT lean_obj_res gcc_fsync(b_lean_obj_arg path) {
  int fd = open(lean_string_cstr(path), O_RDONLY | O_CLOEXEC);
  if (fd < 0) return lean_io_result_mk_error(lean_decode_io_error(errno, path));
  int r;
  do { r = fsync(fd); } while (r < 0 && errno == EINTR);
  int err = errno;
  close(fd);
  if (r < 0) return lean_io_result_mk_error(lean_decode_io_error(err, path));
  return lean_io_result_mk_ok(lean_box(0));
}
