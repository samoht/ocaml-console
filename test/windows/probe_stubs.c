/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>

#ifdef _WIN32
#include <windows.h>
#endif

/* Whether the console on standard output interprets escape sequences:
   1 or 0, or -1 when standard output is no Windows console. */

CAMLprim value probe_vt(value unit) {
  (void)unit;
#ifdef _WIN32
  DWORD mode;
  if (GetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), &mode))
    return Val_int((mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) ? 1 : 0);
#endif
  return Val_int(-1);
}
