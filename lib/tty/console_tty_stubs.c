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

/* Lets a Windows console interpret escape sequences on [fd]. Returns
   [false] when [fd] is no Windows console. */

CAMLprim value console_tty_enable_vt(value fd) {
#ifdef _WIN32
  HANDLE h = Handle_val(fd);
  DWORD mode;
  if (GetConsoleMode(h, &mode)
      && SetConsoleMode(h, mode | ENABLE_PROCESSED_OUTPUT
                               | ENABLE_VIRTUAL_TERMINAL_PROCESSING))
    return Val_true;
#else
  (void)fd;
#endif
  return Val_false;
}

/* The code page of the console's output, or -1 without a Windows console. */

CAMLprim value console_tty_output_code_page(value unit) {
  (void)unit;
#ifdef _WIN32
  UINT cp = GetConsoleOutputCP();
  if (cp != 0) return Val_long(cp);
#endif
  return Val_long(-1);
}

/* Sets the code page of the console's output. Returns [false] on failure. */

CAMLprim value console_tty_set_output_code_page(value cp) {
#ifdef _WIN32
  if (SetConsoleOutputCP((UINT)Long_val(cp))) return Val_true;
#else
  (void)cp;
#endif
  return Val_false;
}
