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
(** The type for how a display shows progress. Constructors may be added, so a
    match on a progress has a case for the others. *)

val all : (string * t) list
(** [all] is every progress under its name, [`Auto] first, which are the choices
    of a [--progress] option as [Cmdliner.Arg.enum all] reads them:
    [[("auto", `Auto); ("tty", `Tty); ("plain", `Plain); ("quiet", `Quiet);
     ("json", `Json)]]. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] is the progress of {!all} named [s]. The error message lists
    the names. *)

(** {1:json JSON events}

    With [`Json] a display writes one JSON object on each line of its formatter
    for each event, in the order the events happen. Objects may gain fields, new
    events may be added, and a field whose values are listed below may gain
    values, but the fields below keep their meaning. A consumer skips an event
    or a value it does not know. The header of the display is not an event.

    Every object has:
    - [time], the seconds since the display started, a number with three
      decimals;
    - [event], one of the strings below.

    An event of a task also has:
    - [id], an integer that names the task in the display: the tasks are
      numbered from [0] in the order they start, and no two tasks of a display
      have the same number;
    - [task], the labels from the top task down to it, an array of strings.

    The events are:
    - ["start"]: the task started. [parent] is the [id] of the task it is under,
      if any.
    - ["progress"]: its count changed. [count] and [total] are integers, and
      [unit] is ["items"] or ["bytes"].
    - ["label"]: its label changed to [text].
    - ["output"]: [text] is a line of its output. [level], only for a [Logs]
      message, is that message's level: ["app"], ["error"], ["warning"],
      ["info"] or ["debug"]. Output outside any task has [task] at [[]] and no
      [id].
    - ["end"]: it ended. [status] is ["succeeded"], ["failed"] or ["cancelled"],
      [elapsed] is the seconds it ran, with three decimals, and [text] is the
      message it ended with, if any.
    - ["result"]: the result of {!Display.set_result}, when the display closes.
      [status] is ["ok"] or ["error"], [text] is its message and [detail] its
      detail, if any.

    A transient task ({!Display.val-task}) has its events as any other task has.
    Strings are valid UTF-8. A control character in them is always escaped. The
    task in the example of {!Display} gives these events:
    {v
    {"time":0.000,"event":"start","id":0,"task":["compile"]}
    {"time":0.000,"event":"progress","id":0,"task":["compile"],"count":1,"total":2,"unit":"items"}
    {"time":0.000,"event":"output","id":0,"task":["compile"],"text":"ocamlopt parser.ml"}
    {"time":0.000,"event":"progress","id":0,"task":["compile"],"count":2,"total":2,"unit":"items"}
    {"time":0.000,"event":"end","id":0,"task":["compile"],"status":"succeeded","elapsed":0.000}
    v} *)
