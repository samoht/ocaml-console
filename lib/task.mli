(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tasks: the work a program reports, without drawing it.

    A task has a label, a count of items or bytes, lines of output, and an end.
    Tasks form a tree under a {!root}, which stands for the work as a whole.
    Ending the root says how the work ended. {!Progress} shows the tasks of a
    root as rows, as lines, as failures only, or as JSON. [Console_eio.run]
    makes a root, shows it, and passes it to the program:

    {[
    let build root =
      Console.Task.run root "compile" (fun step ->
          Console.Task.set_items step ~count:1 ~total:2;
          Console.Task.log step "ocamlopt parser.ml";
          Console.Task.set_items step ~count:2 ~total:2)
    ]}

    Any domain can call the functions of a task. The domain that made the root
    owns the work: every change happens in that domain, and watchers and views
    run there. A call from another domain records its request and returns at
    once. The owner applies the request at its next call on these tasks, or at
    the next redraw of its display. Requests are applied in the order they were
    made, so {!Task.advance} loses no step. Until then, what another domain
    reads may be out of date. An error that only the owner can detect, such as
    {!advance} on a task that counts nothing, is dropped. Past 4 MiB of waiting
    lines, the oldest are dropped, and a line says how many. The [now] function
    of {!root} must be callable from any domain.

    A task that ended ignores every change, and {!log} drops a line given to it.
    Only the root still accepts {!log} after it ended. These lines belong to no
    task. *)

type t = Task_core.t
(** The type for tasks. *)

type level = [ `App | `Error | `Warning | `Info | `Debug ]
(** The type for the levels of a line of output, as in [Logs]. *)

type state = [ `Running | `Succeeded | `Failed | `Cancelled ]
(** The type for the state of a task: running, or how it ended. A task that
    ended has the {!Event.type-status} of the same name in its ["end"] event,
    and the {!Theme.type-mark} of the same name on its line. *)

type progress = [ `Spin | `Items of int * int | `Bytes of int64 * int64 ]
(** The type for the progress of a task. [`Spin] means the task has no count
    yet, and its row shows a spinner. Otherwise, it is a count of items or of
    bytes done, out of a total. *)

(** {1:tree Roots and tasks} *)

val root : ?label:string -> now:(unit -> float) -> unit -> t
(** [root ~now ()] is the work as a whole, timed by the clock [now], in seconds.
    [label], if given, names the work on the summary line ({!Progress.summary}).
    A root is a task. It is the current task wherever no other task is current.
    Its lines belong to no task, and ending it ends the work. *)

val start : ?key:string -> ?log_lines:int -> ?transient:bool -> t -> string -> t
(** [start parent label] starts a task named [label] under [parent], which is a
    root or a task. A line feed in [label] becomes a space.

    - [key], if given, names the task. While the task runs, a later call with
      the same [key] under the same root returns the same task. The [label],
      [parent], [log_lines] and [transient] of that later call are ignored.
    - [log_lines] (defaults to [8]) is the number of latest lines of the task
      that its row shows under it while it runs.
    - [transient] (defaults to [false]) makes a task that shows only while it
      runs. When it succeeds, it leaves neither its own line nor any of its
      output. Everything printed while it is current is its output, [Logs]
      messages included. So print a line that should stay outside the task, or
      after it ended. A transient task that fails or is cancelled shows like any
      other task.

    A task started under a task that ended (the root included) ends at once. It
    succeeds if that task succeeded, and is cancelled otherwise.

    @raise Invalid_argument if [key] is empty or [log_lines] is negative. *)

val run :
  ?key:string ->
  ?log_lines:int ->
  ?transient:bool ->
  t ->
  string ->
  (t -> 'a) ->
  'a
(** [run parent label f] is [f task], where [task] is started as {!start} does
    and is current while [f] runs ({!within}). The task succeeds when [f]
    returns. When [f] raises [e], the task fails with the message
    [label task ^ ": " ^ Printexc.to_string e] ({!fail}), and [e] is raised
    again. If [f] ended the task itself, that end stays. Under
    [Console_eio.run], a cancelled fiber cancels the task instead of failing it.

    When [key] names a running task, [run] is [within task (fun () -> f task)]
    for that task. It ends nothing: the call that started the task ends it. *)

val within : t -> (unit -> 'a) -> 'a
(** [within t f] is [f ()], with [t] current while [f] runs. Under
    [Console_eio.run], [t] is also current in the fibers that [f] starts. The
    current task decides only where output goes: what [f] prints, and logs with
    [Logs], is the output of [t]. Unlike {!run}, [within] starts no task and
    ends none, even when [f] raises. *)

(** {1:changes Changes} *)

val set_label : t -> string -> unit
(** [set_label t label] renames [t]. The root keeps its label. *)

val set_items : t -> count:int -> total:int -> unit
(** [set_items t ~count ~total] says that [count] of [total] items are done. A
    count never goes back: a [count] or [total] smaller than before is ignored.
    A [count] above [total] raises the total.

    It does nothing on the root or on a task that ended. The summary line of the
    root counts its tasks ({!Progress.summary}), so a count of the whole work
    belongs to a task started for it. This also applies to a library that calls
    a nested [Console_eio.run] and gets the root: it starts a task to count.

    @raise Invalid_argument
      if [count] or [total] is negative, or [t] counts bytes. *)

val set_bytes : t -> count:int64 -> total:int64 -> unit
(** [set_bytes t ~count ~total] is like {!set_items}, for a task that counts
    bytes. Its row also shows the bytes per second. The root counts nothing.

    @raise Invalid_argument
      if [count] or [total] is negative, or [t] counts items. *)

val advance : ?by:int -> t -> unit
(** [advance t] adds [by] (defaults to [1]) to the count of [t], in items or
    bytes, and keeps its total. For a task of items, it is
    [set_items t ~count:(count + by) ~total], and likewise with {!set_bytes} for
    a task of bytes. So a count past the total raises the total. Like
    {!set_items}, it does nothing on the root or on a task that ended.

    @raise Invalid_argument
      if [by] is negative, or if [t] is running and has no count yet. Give its
      total first, with {!set_items} or {!set_bytes}. *)

val log : ?level:level -> t -> string -> unit
(** [log t text] adds each line of [text] to the output of [t]. With [level],
    each line is a [Logs] message at that level. Without [level], each line is a
    line the program printed. [log] is a shorter form of {!within}, for code
    where [t] is not current.

    - A tab moves to the next stop of eight columns from the start of its line.
    - A running task keeps its latest lines, which its row shows. It also keeps
      up to 1 MiB of all its lines, to show if it fails. Past that, the oldest
      lines go first, and a line counts them. A line, a label and a message are
      each kept up to 4 KiB. A longer one is cut between two characters and ends
      with three dots.
    - The lines under the row of a running task are drawn in the muted colour of
      the theme ({!Theme.muted}), except warnings and errors. A line printed to
      the scrollback, such as the output of a failed task, is not muted.
    - The lines of the root belong to no task.
    - [log] drops a line given to a task that ended. A [Logs] message or a
      printed line from a fiber whose task ended is not dropped. It goes to the
      scrollback at once, labelled with that task ({!Progress.design}). *)

val succeed : ?message:string -> t -> unit
(** [succeed t] ends [t] as a success, with [message] in place of its label if
    given. A task ends only once: the first call to {!succeed}, {!fail} or
    {!cancel} counts, and later calls do nothing. The program decides how a task
    ends: a subtask of [t] that failed does not fail [t].

    The subtasks of [t] that still run end first, in the order they started.
    They succeed if [t] succeeds, and are cancelled if [t] fails or is
    cancelled. On the root, this says how the work ended. *)

val fail : ?message:string -> t -> unit
(** [fail t] is like {!succeed} but ends [t] as a failure. *)

val cancel : ?message:string -> t -> unit
(** [cancel t] is like {!succeed} but ends [t] as cancelled. *)

(** {1:read Accessors} *)

val label : t -> string
(** [label t] is the label of [t] as it shows: on one line, at most 4 KiB. *)

val parent : t -> t option
(** [parent t] is the task [t] was started under, or [None] for a root. *)

val children : t -> t list
(** [children t] are the tasks under [t], in the order they started: those that
    run, and those that ended and still show ({!Progress}). A transient task
    that succeeded is not among them. Its subtasks take its place, as in
    {!Progress.tree}. *)

val path : t -> string list
(** [path t] is the labels from the top task down to [t], or [[]] for a root. *)

val state : t -> state
(** [state t] is the state of [t]. *)

val progress : t -> progress
(** [progress t] is how much of [t] is done. *)

val message : t -> string option
(** [message t] is the message [t] ended with, if any. *)

val elapsed : t -> float
(** [elapsed t] is the number of seconds [t] ran for, or has run for so far. *)

val rate : t -> float option
(** [rate t] is the items or bytes per second that [t] counts, as
    {!Progress.Layout.rate} shows it. It is a moving average that weights a rate
    measured [dt] seconds ago by [exp (-dt /. 2.)]. So it depends on time, not
    on how often the count is given. It is [None] until a count given after [t]
    started shows progress. *)

val output : t -> string list
(** [output t] is the lines of output that [t] keeps, oldest first, each as
    {!log} keeps it. These are the latest lines, within 1 MiB. A task that
    failed or was cancelled keeps them until {!Progress} has printed it: when
    its top task moves to the scrollback, or when it ends without a terminal
    ({!Progress.design}). A task that succeeded keeps them only while it ends,
    so that the [commit] of {!Progress.show} can draw them. The root has none
    ({!log}). The last [log_lines] of them ({!start}) are the lines its row
    shows. *)

val dropped : t -> int
(** [dropped t] is the number of the oldest lines of [t] that {!output} no
    longer holds, to stay within 1 MiB. *)

val pp : t Fmt.t
(** [pp] formats a task for diagnostics, in an unspecified format. *)

(** {1:json JSON events}

    Each change of a task is an event. {!on_event} gives the events to a
    program. [`Json] ({!Progress}) writes each event as one JSON object on its
    own line, in the order the events happen. The line is the {!Event.to_string}
    of an {!Event.t}, and {!Event.jsont} reads it back. The summary line is not
    an event.

    Objects may gain members, new events may be added, and a member with a
    listed set of values may gain values. But the members of {!Event} keep their
    meaning. A consumer skips any member, event or value it does not know, as
    {!Event.jsont} does. An event with another name may give a listed member
    another type: {!Event.jsont} skips that member too. *)

(** JSON events.

    An event is a JSON object with two members:
    - [time]: the seconds since the root started, a number rounded to the
      millisecond, such as [1.5] or [0] ({!Event.time}). A time is kept within
      \[[-10{^ 12}];[10{^ 12}]\] seconds. A larger magnitude becomes the bound,
      and a time that is not finite is [0];
    - [event]: the name of the event ({!Event.type-kind}).

    An event of a task also has:
    - [id]: an integer that names the task in its work. Tasks are numbered from
      [0] in the order they start, and no two tasks of a root have the same
      number;
    - [task]: the labels from the top task down to the task, as an array of
      strings.

    An event that belongs to no task has neither. Examples are output outside
    any task, and the ["end"] of the work as a whole.

    The other members depend on the event, in the order of
    {{!Event.members}its accessors}. A member that an event does not have is
    absent. Strings are valid UTF-8 and hold no C1 control (U+0080 to U+009F),
    since a terminal may read one as an escape code. Each C1 control is replaced
    by U+FFFD. So is each maximal subpart of an ill-formed sequence, as
    [String.get_utf_8_uchar] decodes it. The C0 controls and DEL are escaped.
    Numbers have at most 15 significant digits. So an integer, which is never
    negative, is a JSON number below 10{^ 15}, and a string of decimal digits
    from 10{^ 15} on.

    A label (in [task] or in ["label"]) and the [message] of ["end"] are at most
    4 KiB. A longer one is cut between two characters and ends with [...]
    ({!log}). A line of output is whole, up to the 64 KiB pieces in which
    [Console_eio.run] reads the output of the program.

    The task of the example above gives these events, then the work ends:
    {v
    {"time":0,"event":"start","id":0,"task":["compile"]}
    {"time":0,"event":"progress","id":0,"task":["compile"],"count":1,"total":2,"unit":"items"}
    {"time":0,"event":"output","id":0,"task":["compile"],"text":"ocamlopt parser.ml"}
    {"time":0,"event":"progress","id":0,"task":["compile"],"count":2,"total":2,"unit":"items"}
    {"time":0,"event":"end","id":0,"task":["compile"],"status":"succeeded","elapsed":0}
    {"time":0,"event":"end","status":"succeeded","elapsed":0}
    v}

    A program reads a line with {!Event.of_string}, or with any
    {{:https://erratique.ch/software/jsont}jsont} decoder of {!Event.jsont}. *)
module Event : sig
  (** {1:values Values}

      The types for event names and member values are open: constructors may be
      added. A value read as [`Other s] today may read as a constructor of its
      own later, so a match on one needs a catch-all case. An event never holds
      [`Other s] where [s] is a listed name: the constructors and {!jsont} turn
      [s] into the constructor of that name. *)

  type kind =
    [ `Start  (** ["start"]: the task started. *)
    | `Progress  (** ["progress"]: the count of the task changed. *)
    | `Label  (** ["label"]: the label of the task changed. *)
    | `Output  (** ["output"]: a line of output, of a task or of none. *)
    | `End
      (** ["end"]: the task ended, or the work as a whole ended (it has no
          task). *)
    | `Other of string  (** An event of another name. *) ]
  (** The type for the names of events: the [event] member. *)

  type count_unit = [ `Items | `Bytes | `Other of string ]
  (** The type for the units of a count: the [unit] member, ["items"], ["bytes"]
      or another string. *)

  type nonrec level = [ level | `Other of string ]
  (** The type for the levels of Logs messages: the [level] member, ["app"],
      ["error"], ["warning"], ["info"], ["debug"] or another string. *)

  type status = [ `Succeeded | `Failed | `Cancelled | `Other of string ]
  (** The type for how a task or the work ended: the [status] member of ["end"],
      ["succeeded"], ["failed"], ["cancelled"] or another string. *)

  type task = int * string list
  (** The type for the task of an event: its [id] member, and its [task] member
      (the labels from the top task down to it). *)

  type t = Event.t
  (** The type for events. A transient task ({!start}) has the same events as
      any other task. *)

  (** {1:make Constructors}

      Each event has a constructor that takes its members. The members that an
      event may lack are optional arguments, so a member added later is also an
      optional argument. The task of an event (its [id] and its [task], the
      labels from the top task down to it) is the last positional argument,
      except in {!Event.label}, where the text comes after it. [time] and
      [elapsed] are taken as {!jsont} writes them, and each string as valid
      UTF-8 without a C1 control.

      The constructors raise [Invalid_argument] if given a negative [id],
      [parent], [count] or [total]. *)

  val start : ?parent:int -> time:float -> task -> t
  (** [start ~parent ~time task] is ["start"]: [task] started, under the task
      whose [id] is [parent], if given. *)

  val progress :
    time:float -> count:int64 -> total:int64 -> unit:count_unit -> task -> t
  (** [progress ~time ~count ~total ~unit task] is ["progress"]: the count of
      [task] changed to [count] of [total], in [unit]. Each call to {!set_items}
      or {!set_bytes} that changes them is one event, however often they are
      called. *)

  val label : time:float -> task -> string -> t
  (** [label ~time task text] is ["label"]: the label of [task] changed to
      [text]. *)

  val output : ?task:task -> ?level:level -> time:float -> string -> t
  (** [output ~task ~level ~time text] is ["output"]: [text] is a line of output
      of [task]. Without [task], the line belongs to no task. [level] is the
      level of a Logs message. *)

  val succeeded :
    ?task:task -> ?message:string -> time:float -> elapsed:float -> unit -> t
  (** [succeeded ~task ~message ~time ~elapsed ()] is ["end"] with [status]
      ["succeeded"]: [task] ended after [elapsed] seconds. Without [task], the
      work as a whole ended. [message] is the message it ended with, if any. *)

  val failed :
    ?task:task -> ?message:string -> time:float -> elapsed:float -> unit -> t
  (** [failed] is {!succeeded} with [status] ["failed"]. *)

  val cancelled :
    ?task:task -> ?message:string -> time:float -> elapsed:float -> unit -> t
  (** [cancelled] is {!succeeded} with [status] ["cancelled"]. *)

  val other : ?task:task -> time:float -> string -> t
  (** [other ~task ~time name] is the event named [name], of [task] if given,
      with no other member.

      @raise Invalid_argument if [name] is the name of a listed {!type-kind}. *)

  (** {1:members Members}

      The members of an event are in the order of these accessors. A member that
      an event does not have is [None], and is absent from its object. *)

  val time : t -> float
  (** [time e] is the seconds since the root started when [e] happened. *)

  val kind : t -> kind
  (** [kind e] is the name of [e]. *)

  val task : t -> task option
  (** [task e] is the task of [e]. It is [None] for output outside any task, for
      the ["end"] of the work, and for an event of another name that has no
      task. *)

  val parent : t -> int option
  (** [parent e] is the [id] of the parent of the task of ["start"], if any. *)

  val count : t -> int64 option
  (** [count e] is the count of ["progress"]. It is written as a JSON number
      below 10{^ 15} in magnitude, and as a string of decimal digits from
      10{^ 15} on. *)

  val total : t -> int64 option
  (** [total e] is the total of ["progress"], written as {!count} is. *)

  val unit : t -> count_unit option
  (** [unit e] is the unit of the count of ["progress"]. *)

  val status : t -> status option
  (** [status e] is how the task or the work of ["end"] ended. *)

  val elapsed : t -> float option
  (** [elapsed e] is the number of seconds that the task or the work of ["end"]
      ran for. *)

  val text : t -> string option
  (** [text e] is the label of ["label"], or the line of ["output"]. *)

  val message : t -> string option
  (** [message e] is the message of ["end"], if any. *)

  val level : t -> level option
  (** [level e] is the level of ["output"], for a Logs message. *)

  (** {1:json JSON} *)

  val jsont : t Jsont.t
  (** [jsont] maps an event to its JSON object.

      - It reads and writes {!val-time} and {!val-elapsed} rounded to the
        millisecond, within \[[-10{^ 12}];[10{^ 12}]\]. A larger magnitude
        becomes the bound, and a value that is not finite becomes [0]. So each
        value reads back as it was written.
      - It encodes an integer of 10{^ 15} or more as a string, and each string
        as valid UTF-8 without a C1 control.
      - It decodes an event of another name as [`Other], with its {!val-time},
        and with its [id] and [task] if they have the types of
        {{!json}an event of a task}. Its other members are skipped, whatever
        their types.
      - It decodes a value of a {!type-count_unit}, a {!type-level} or a
        {!type-status} that it does not list as [`Other]. It skips the members
        it does not know.
      - An event with a listed name does not decode if it lacks a member of its
        kind, or if a listed member has another type. Neither does a negative or
        fractional integer. *)

  val of_string : string -> (t, [ `Msg of string ]) result
  (** [of_string s] is the event of the JSON object [s], decoded with {!jsont}.
      It is [Error (`Msg m)] if [s] is not the JSON object of an event, with [m]
      in plain text, without escape codes. *)

  val to_string : t -> string
  (** [to_string e] is the line that [`Json] writes for [e], without the line
      feed. It is the object of {!jsont} on one line, with its members in order,
      written by [Jsont_bytesrw.encode_string] with 15 significant digits for
      numbers. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an event as {!to_string} does. *)
end

val on_event : t -> (Event.t -> unit) -> unit
(** [on_event t f] calls [f] on each event of the work of [t], from now on, in
    the order the events happen. This covers every task under the root, and the
    root itself. [f] runs inside the call that makes the change, and must not
    change a task. A program can write its own lines from the events, as [`Json]
    writes JSON objects. *)
