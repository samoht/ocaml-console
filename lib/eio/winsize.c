#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <sys/ioctl.h>
#include <unistd.h>

CAMLprim value caml_console_winsize(value v_fd) {
  struct winsize ws;
  int fd = Int_val(v_fd);
  if (ioctl(fd, TIOCGWINSZ, &ws) == -1)
    caml_failwith("TIOCGWINSZ");
  return Val_int(ws.ws_col);
}

CAMLprim value caml_console_winsize_rows(value v_fd) {
  struct winsize ws;
  int fd = Int_val(v_fd);
  if (ioctl(fd, TIOCGWINSZ, &ws) == -1)
    caml_failwith("TIOCGWINSZ");
  return Val_int(ws.ws_row);
}
