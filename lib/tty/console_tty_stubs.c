/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>

#ifndef _WIN32
#include <sys/ioctl.h>
#endif

/* Returns the columns and rows of a terminal packed in one integer, or -1. */

CAMLprim value console_tty_window_size(value fd) {
#ifdef TIOCGWINSZ
  struct winsize w;
  if (ioctl(Int_val(fd), TIOCGWINSZ, &w) == 0)
    return Val_long(((long)w.ws_col << 16) | w.ws_row);
#else
  (void)fd;
#endif
  return Val_long(-1);
}
