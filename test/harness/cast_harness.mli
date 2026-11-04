(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A reader of {{:https://docs.asciinema.org/manual/asciicast/v2/}asciicast v2}
    that shares no code with the writer under test. It compares lines as JSON
    values, so any text that encodes the same JSON matches. *)

val json : Jsont.json Alcotest.testable
(** [json] compares JSON values, not the text that writes them. *)

val parse : string -> Jsont.json
(** [parse line] is the JSON value of [line], which fails the test if [line] is
    not valid UTF-8 or not one JSON value. *)

type event = { time : float; code : string; data : string }
(** The type for lines of the event stream: [[time, code, data]]. *)

val event : string -> event
(** [event line] is the event [line] writes, which fails the test if [line] is
    not a three-element array of a number and two strings. *)

val events : string list -> event list
(** [events lines] are the events of the lines of a recording, after its header.
*)
