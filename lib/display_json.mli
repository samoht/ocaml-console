(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The JSON text of a display's events
    ({{:https://www.rfc-editor.org/rfc/rfc8259}RFC 8259}). *)

val add_string : Buffer.t -> string -> unit
(** [add_string buf s] adds [s] to [buf] as a JSON string. Each byte of [s] that
    is not part of valid UTF-8 is the replacement character U+FFFD; the quote,
    the backslash and every control character (C0, DEL and C1) are escaped, so
    the string is valid UTF-8 on one line. *)

val add_number : Buffer.t -> float -> unit
(** [add_number buf x] adds [x] to [buf] with three decimals, [0] if [x] is not
    finite. *)
