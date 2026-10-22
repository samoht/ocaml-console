(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** How a display shows progress.

    A program shows its progress as rows redrawn in place, as lines printed in
    turn, as only what fails, or as JSON events, the choices that the
    [--progress] option of [docker build] gives. {!Display.v} takes one, and so
    does [Console_eio.run], which also finds out whether the output is a
    terminal. *)

type t =
  [ `Auto
    (** The rows are redrawn in place on a terminal, and lines are printed in
        turn otherwise. *)
  | `Tty
    (** The rows are redrawn in place on any output, such as a pipe that a
        terminal recorder reads. *)
  | `Plain
    (** Lines are printed in turn, as on a pipe. A task prints a line when it
        starts, its output at once and a line when it ends. The header comes
        last. The lines are in colour if the output has colour. *)
  | `Quiet
    (** Only what fails is printed. There are no rows, no task lines and no
        header. A task that fails or is cancelled prints its line, then its
        output under it, as on a terminal ({!Display.output}). What goes above
        the rows is printed as usual, the result of {!Display.set_result}
        included. *)
  | `Json
    (** Each event is one JSON object on a line of its own, and nothing else is
        printed ({{!json}JSON events}). *) ]
(** The type for progress modes, how a display shows progress. Constructors may
    be added, so a match on a progress mode has a case for the others. *)

val all : (string * t) list
(** [all] is every progress mode under its name, [`Auto] first, which are the
    choices of a [--progress] option as [Cmdliner.Arg.enum all] reads them:
    [[("auto", `Auto); ("tty", `Tty); ("plain", `Plain); ("quiet", `Quiet);
     ("json", `Json)]]. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] is the progress mode of {!all} named [s]. The error message
    lists the names. *)

val to_string : t -> string
(** [to_string p] is the name of [p] in {!all}. *)

(** {1:json JSON events}

    With [`Json] a display writes one JSON object on each line of its formatter
    for each event, in the order the events happen: the line is the
    {!Event.to_string} of an {!Event.t}, and {!Event.jsont} reads it back. The
    header of the display is not an event.

    Objects may gain members, new events may be added, and a member whose values
    are listed may gain values, but the members of {!Event} keep their meaning.
    A consumer skips a member, an event or a value it does not know, as
    {!Event.jsont} does. *)

(** JSON events.

    An event is a JSON object with two members:
    - [time], the seconds since the display started, a number rounded to the
      millisecond, such as [1.5] or [0] ({!Event.time});
    - [event], the name of the event ({!Event.type-kind}).

    An event of a task also has:
    - [id], an integer that names the task in the display: the tasks are
      numbered from [0] in the order they start, and no two tasks of a display
      have the same number;
    - [task], the labels from the top task down to it, an array of strings.

    The other members are those of each event, in the order {!Event.type-kind}
    gives them, a member that an event may not have being absent. Strings are
    valid UTF-8 and hold no C1 control (U+0080 to U+009F), which a terminal may
    take for an escape code: each byte that is not part of a character and each
    C1 control is U+FFFD. The C0 controls and DEL are escaped. Numbers have at
    most 15 significant digits, so an integer is a JSON number between
    -10{^ 15}+1 and 10{^ 15}-1, and a string of decimal digits beyond.

    The task in the example of {!Display} gives these events:
    {v
    {"time":0,"event":"start","id":0,"task":["compile"]}
    {"time":0,"event":"progress","id":0,"task":["compile"],"count":1,"total":2,"unit":"items"}
    {"time":0,"event":"output","id":0,"task":["compile"],"text":"ocamlopt parser.ml"}
    {"time":0,"event":"progress","id":0,"task":["compile"],"count":2,"total":2,"unit":"items"}
    {"time":0,"event":"end","id":0,"task":["compile"],"status":"succeeded","elapsed":0}
    v}

    A program reads a line with {!Event.of_string}, or with any
    {{:https://erratique.ch/software/jsont}jsont} decoder of {!Event.jsont}. *)
module Event : sig
  (** {1:values Values} *)

  type task = int * string list
  (** The type for the task of an event: its [id] and its [task], the labels
      from the top task down to it. *)

  type count_unit = [ `Items | `Bytes | `Other of string ]
  (** The type for the units of a count, the [unit] member: ["items"], ["bytes"]
      or another string. *)

  type level = [ `App | `Error | `Warning | `Info | `Debug | `Other of string ]
  (** The type for the levels of Logs messages, the [level] member: ["app"],
      ["error"], ["warning"], ["info"], ["debug"] or another string. *)

  type status = [ `Succeeded | `Failed | `Cancelled | `Other of string ]
  (** The type for how a task ended, the [status] member of ["end"]:
      ["succeeded"], ["failed"], ["cancelled"] or another string. *)

  type outcome = [ `Ok | `Error | `Other of string ]
  (** The type for how the work ended, the [status] member of ["result"]:
      ["ok"], ["error"] or another string. *)

  type kind =
    [ `Start of task * int option
      (** ["start"]: the task started, under the task whose [id] is [parent], if
          any. *)
    | `Progress of task * int64 * int64 * count_unit
      (** ["progress"]: the count of the task changed to [count] of [total], in
          [unit]. A [count] or a [total] of 10{^ 15} or more is a string of
          decimal digits. Each call to {!Display.set_count} or
          {!Display.set_bytes} that changes them is one event, however often it
          comes. *)
    | `Label of task * string
      (** ["label"]: the label of the task changed to [text]. *)
    | `Output of task option * string * level option
      (** ["output"]: [text] is a line of output of the task, and [level], only
          for a Logs message, its level. Output outside any task has no [id] and
          [task] at [[]]. *)
    | `End of task * status * float * string option
      (** ["end"]: the task ended with [status], after [elapsed] seconds,
          rounded to the millisecond, and [text] is the message it ended with,
          if any. *)
    | `Result of outcome * string * string option
      (** ["result"]: the result of {!Display.set_result}, when the display
          closes. It has no [id] nor [task]: [status] is the outcome, [text] its
          message and [detail] its detail, if any. *)
    | `Other of string
      (** An event of another name. The line of [`Other n], [n] one of the names
          above, does not decode. *) ]
  (** The type for the kinds of events and their members. A transient task
      ({!Display.val-task}) has its events as any other task has. *)

  type t
  (** The type for events. *)

  val make : time:float -> kind -> t
  (** [make ~time k] is the event [k] at [time] seconds. *)

  val time : t -> float
  (** [time e] is the seconds since the display started when [e] happened. *)

  val kind : t -> kind
  (** [kind e] is the kind of [e] and its members. *)

  (** {1:json JSON} *)

  val jsont : t Jsont.t
  (** [jsont] maps an event to its JSON object. It encodes [time] and [elapsed]
      rounded to the millisecond, [0] if not finite, an integer of 10{^ 15} or
      more as a string, and each string as valid UTF-8 without a C1 control. It
      decodes an event of another name as [`Other], a value of a
      {!type-count_unit}, a {!type-level}, a {!type-status} or an
      {!type-outcome} it does not list as [`Other], and skips the members it
      does not know. An event that lacks a member of its kind does not decode.
  *)

  val of_string : string -> (t, [ `Msg of string ]) result
  (** [of_string s] is the event of the JSON object [s], decoded with {!jsont}.
  *)

  val to_string : t -> string
  (** [to_string e] is the line a display writes for [e], without the line feed:
      the object of {!jsont} on one line, its members in order, written by
      [Jsont_bytesrw.encode_string] with 15 significant digits for numbers. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an event as {!to_string} does. *)
end
