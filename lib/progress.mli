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
        starts, its output at once and a line when it ends, each naming the task
        by its path ({!Display.design}). The display's view comes last, once: by
        default its summary line. The lines are in colour if the output has
        colour. *)
  | `Quiet
    (** Only what fails is printed: a task that fails or is cancelled prints its
        line, which names its path, then all its output under it
        ({!Display.output}). A task that succeeds prints nothing, and there are
        no rows and no summary. What goes above the rows is printed as usual,
        the result of {!Display.set_result} included. *)
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
    summary of the display is not an event.

    Objects may gain members, new events may be added, and a member whose values
    are listed may gain values, but the members of {!Event} keep their meaning.
    A consumer skips a member, an event or a value it does not know, as
    {!Event.jsont} does. An event of another name may give a listed member
    another type: {!Event.jsont} then skips it too. *)

(** JSON events.

    An event is a JSON object with two members:
    - [time], the seconds since the display started, a number rounded to the
      millisecond, such as [1.5] or [0] ({!Event.time}). A time is held in
      \[[-10{^ 12}];[10{^ 12}]\] seconds: a larger magnitude is the bound, and a
      time that is not finite is [0];
    - [event], the name of the event ({!Event.type-kind}).

    An event of a task also has:
    - [id], an integer that names the task in the display: the tasks are
      numbered from [0] in the order they start, and no two tasks of a display
      have the same number;
    - [task], the labels from the top task down to it, an array of strings.

    An event of no task, such as output outside any task or ["result"], has
    neither.

    The other members are those of each event, in the order of
    {{!Event.members}its accessors}, a member that an event does not have being
    absent. Strings are valid UTF-8 and hold no C1 control (U+0080 to U+009F),
    which a terminal may take for an escape code: each C1 control is U+FFFD, and
    so is each maximal subpart of an ill-formed sequence, as
    [String.get_utf_8_uchar] decodes it. The C0 controls and DEL are escaped.
    Numbers have at most 15 significant digits, so an integer, never negative,
    is a JSON number below 10{^ 15}, and a string of decimal digits from there.

    A label, in [task] or in ["label"], and the message of ["end"] are at most 4
    KiB: a longer one is cut between two characters and ends with [...]
    ({!Display.output}). A line of output is whole, up to the pieces of 64 KiB
    in which [Console_eio.run] reads the program's output.

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
  (** {1:values Values}

      The types of the names of events and of the values of their members are
      open: constructors may be added, so a value read as [`Other s] today may
      read as a constructor of its own later, and a match on one has a case for
      the others. An event never holds [`Other s] with [s] a listed name: the
      constructors take it as that name's constructor, and {!jsont} reads [s] as
      it. *)

  type kind =
    [ `Start  (** ["start"]: the task started. *)
    | `Progress  (** ["progress"]: the count of the task changed. *)
    | `Label  (** ["label"]: the label of the task changed. *)
    | `Output  (** ["output"]: a line of output, of a task or of none. *)
    | `End  (** ["end"]: the task ended. *)
    | `Result
      (** ["result"]: the result of {!Display.set_result}, when the display
          closes. *)
    | `Other of string  (** An event of another name. *) ]
  (** The type for the names of events, the [event] member. *)

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

  type t
  (** The type for events. A transient task ({!Display.val-task}) has its events
      as any other task has. *)

  (** {1:make Constructors}

      Each event has a constructor that takes its members, those it may lack as
      optional arguments, so that a member added later is an optional argument.
      The task of an event is its [id] and its [task], the labels from the top
      task down to it, the last argument. [time] and [elapsed] are taken as
      {!jsont} writes them, and each string as valid UTF-8 without a C1 control.

      The constructors raise [Invalid_argument] if given a negative [id],
      [parent], [count] or [total]. *)

  val start : ?parent:int -> time:float -> id:int -> string list -> t
  (** [start ~parent ~time ~id task] is ["start"]: the task started, under the
      task whose [id] is [parent], if any. *)

  val progress :
    time:float ->
    id:int ->
    count:int64 ->
    total:int64 ->
    unit:count_unit ->
    string list ->
    t
  (** [progress ~time ~id ~count ~total ~unit task] is ["progress"]: the count
      of the task changed to [count] of [total], in [unit]. Each call to
      {!Display.set_count} or {!Display.set_bytes} that changes them is one
      event, however often it comes. *)

  val label : time:float -> id:int -> text:string -> string list -> t
  (** [label ~time ~id ~text task] is ["label"]: the label of the task changed
      to [text]. *)

  val output :
    ?task:int * string list -> ?level:level -> time:float -> string -> t
  (** [output ~task ~level ~time text] is ["output"]: [text] is a line of output
      of the task whose [id] and labels are [task], or of no task if [task] is
      absent, and [level], only for a Logs message, its level. *)

  val end' :
    ?text:string ->
    time:float ->
    id:int ->
    status:status ->
    elapsed:float ->
    string list ->
    t
  (** [end' ~text ~time ~id ~status ~elapsed task] is ["end"]: the task ended
      with [status], after [elapsed] seconds, and [text] is the message it ended
      with, if any. *)

  val result : ?detail:string -> time:float -> outcome:outcome -> string -> t
  (** [result ~detail ~time ~outcome text] is ["result"]: the work ended with
      [outcome], [text] is its message and [detail] its detail, if any. It has
      no [id] nor [task], and its [outcome] is its [status] member. *)

  val other : ?id:int -> ?task:string list -> time:float -> string -> t
  (** [other ~id ~task ~time name] is the event named [name], of the task [id]
      and [task] if given, with no other member.

      @raise Invalid_argument if [name] is the name of a listed {!type-kind}. *)

  (** {1:members Members}

      An event's members are in the order of these accessors. A member that an
      event does not have is [None], and is absent from its object. *)

  val time : t -> float
  (** [time e] is the seconds since the display started when [e] happened. *)

  val kind : t -> kind
  (** [kind e] is the name of [e]. *)

  val id : t -> int option
  (** [id e] is the [id] of the task of [e]: [None] for output outside any task,
      for ["result"] and for an event of another name that has none. *)

  val task : t -> string list
  (** [task e] is the labels from the top task of [e] down to its task: [[]]
      where it has none. *)

  val parent : t -> int option
  (** [parent e] is the [id] of the task over that of ["start"], if any. *)

  val count : t -> int64 option
  (** [count e] is the count of ["progress"]. It is written as a JSON number
      below 10{^ 15} in magnitude, and as a string of decimal digits from there.
  *)

  val total : t -> int64 option
  (** [total e] is the total of ["progress"], written as {!count} is. *)

  val unit : t -> count_unit option
  (** [unit e] is the unit of the count of ["progress"]. *)

  val status : t -> status option
  (** [status e] is how the task of ["end"] ended. *)

  val outcome : t -> outcome option
  (** [outcome e] is how the work of ["result"] ended, its [status] member. *)

  val elapsed : t -> float option
  (** [elapsed e] is the seconds the task of ["end"] ran for. *)

  val text : t -> string option
  (** [text e] is the label of ["label"], the line of ["output"], the message of
      ["end"], if any, and that of ["result"]. *)

  val level : t -> level option
  (** [level e] is the level of ["output"], for a Logs message. *)

  val detail : t -> string option
  (** [detail e] is the detail of ["result"], if any. *)

  (** {1:json JSON} *)

  val jsont : t Jsont.t
  (** [jsont] maps an event to its JSON object. It reads and writes [time] and
      [elapsed] rounded to the millisecond in \[[-10{^ 12}];[10{^ 12}]\], a
      larger magnitude as the bound and [0] if not finite, so that each reads
      back as it was written. It encodes an integer of 10{^ 15} or more as a
      string, and each string as valid UTF-8 without a C1 control. It decodes an
      event of another name as [`Other] with its [time], and its [id] and [task]
      if they have the types of {{!json}an event of a task}: its other members,
      whatever their types, are skipped. It decodes a value of a
      {!type-count_unit}, a {!type-level}, a {!type-status} or an
      {!type-outcome} it does not list as [`Other], and skips the members it
      does not know. An event of a listed name that lacks a member of its kind,
      or has a listed member of another type, does not decode, and neither does
      a negative or fractional integer. *)

  val of_string : string -> (t, [ `Msg of string ]) result
  (** [of_string s] is the event of the JSON object [s], decoded with {!jsont}.
      This is [Error (`Msg m)] if [s] is not the JSON object of an event, with
      [m] plain text, without escape codes. *)

  val to_string : t -> string
  (** [to_string e] is the line a display writes for [e], without the line feed:
      the object of {!jsont} on one line, its members in order, written by
      [Jsont_bytesrw.encode_string] with 15 significant digits for numbers. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an event as {!to_string} does. *)
end
