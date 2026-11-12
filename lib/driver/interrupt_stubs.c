/*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <signal.h>

/* Exported by the runtime, declared only under CAML_INTERNALS. */
CAMLextern int caml_convert_signal_number(int);

/* Returns the address of the kernel's handler for a signal. Returns 0 for the
   default disposition, and always on Windows. */

CAMLprim value console_driver_disposition(value signo) {
  intnat h = 0;
#ifndef _WIN32
  struct sigaction sa;
  if (sigaction(caml_convert_signal_number(Int_val(signo)), NULL, &sa) == 0) {
    if (sa.sa_flags & SA_SIGINFO)
      h = (intnat)sa.sa_sigaction;
    else if (sa.sa_handler != SIG_DFL)
      h = (intnat)sa.sa_handler;
  }
#else
  (void)signo;
#endif
  return caml_copy_nativeint(h);
}

/* Sends a signal to the calling thread. */
CAMLprim value console_driver_raise(value signo) {
  raise(caml_convert_signal_number(Int_val(signo)));
  return Val_unit;
}
