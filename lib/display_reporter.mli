(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Logs reporting around a live region.

    While a display owns the terminal, the {!Logs} reporter is wrapped so that
    each report is written with the region {{!Display_term.suspend}suspended}. A
    program never has to know that a log line must clear a live region. *)

val install : unit -> unit
(** [install ()] wraps the current reporter. It does nothing if a wrapper is
    already installed, so successive displays do not nest wrappers. *)

val uninstall : unit -> unit
(** [uninstall ()] puts back the reporter {!install} wrapped, if any. *)
