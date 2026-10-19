(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Display history events.

    This is the representation behind {!Display.Event}. *)

type level = [ `Info | `Warning | `Error ]
(** The type for message severities. *)

type outcome = [ `Succeeded | `Failed | `Cancelled ]
(** The type for task outcomes. *)

type result = [ `Ok | `Fail ]
(** The type for session verdicts. *)

type kind =
  [ `Message of level
  | `Started
  | `Log of level
  | `Finished of outcome
  | `Result of result ]
(** The type for event kinds. *)

type t = {
  elapsed : float;  (** Seconds since the session started. *)
  task : int option;  (** The task's numeric identity. *)
  key : string option;  (** The task's caller-given identity. *)
  path : string list;  (** The task labels from the root down. *)
  kind : kind;
  message : string;  (** The sanitized message. *)
  detail : string option;  (** The verdict's detail. *)
}
(** The type for events. *)

val elapsed : t -> float
(** [elapsed e] is [e.elapsed]. *)

val task : t -> int option
(** [task e] is [e.task]. *)

val key : t -> string option
(** [key e] is [e.key]. *)

val path : t -> string list
(** [path e] is [e.path]. *)

val kind : t -> kind
(** [kind e] is [e.kind]. *)

val message : t -> string
(** [message e] is [e.message]. *)

val detail : t -> string option
(** [detail e] is [e.detail]. *)

val pp : t Fmt.t
(** [pp] formats an event for diagnostics, in an unspecified format. *)
