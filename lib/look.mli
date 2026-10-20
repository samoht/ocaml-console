(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** How a formatter draws what is printed on it. *)

type t = { theme : Theme.t; utf_8 : bool }
(** The type for what a formatter says of how to draw: its theme, and whether it
    shows UTF-8 or ASCII alone. *)

val v : Format.formatter -> t
(** [v ppf] is the look of [ppf]: its {!Theme.get} and its {!Fmt.utf_8}. *)

val default : t
(** [default] is the look of a formatter given nothing: {!Theme.default}, in
    UTF-8. *)

val pp : t Fmt.t
(** [pp] formats a look for diagnostics, in an unspecified format. *)
