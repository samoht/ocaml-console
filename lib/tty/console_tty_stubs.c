/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>

#ifdef _WIN32
#include <windows.h>
#include <caml/unixsupport.h>
#else
#include <sys/ioctl.h>
#endif

/* Returns the columns and rows of a terminal packed in one integer, or -1.
   On Windows, it is the visible window of the console screen buffer. */

CAMLprim value console_tty_window_size(value fd) {
#ifdef _WIN32
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (GetConsoleScreenBufferInfo(Handle_val(fd), &info)) {
    long cols = info.srWindow.Right - info.srWindow.Left + 1;
    long rows = info.srWindow.Bottom - info.srWindow.Top + 1;
    return Val_long((cols << 16) | rows);
  }
#elif defined(TIOCGWINSZ)
  struct winsize w;
  if (ioctl(Int_val(fd), TIOCGWINSZ, &w) == 0)
    return Val_long(((long)w.ws_col << 16) | w.ws_row);
#else
  (void)fd;
#endif
  return Val_long(-1);
}
