(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** What console prints: styled text, and everything built from it.

    A view is what is drawn in a given room at a given time. A widget's [view],
    such as {!Table.view}, is a view, and views compose: a table goes in a
    panel, the panel in a cell of another table, beside a tree ({!hcat}) or one
    under the other ({!vcat}). A {!Display} draws a view every frame, in the
    room of the terminal, while printing it ({!pp}, {!to_string}) draws it once,
    at time [0]. Most views look the same at every time, while a {!Spinner.view}
    turns, and so do the frames of a theme that moves ({!Theme.val-motion}).

    A view only holds text that prints. The constructors:
    - replace each control character with a space;
    - replace a line end ({!Console_text.sanitize}'s) with a line feed;
    - replace each malformed UTF-8 sequence with U+FFFD;
    - remove the characters that a terminal draws as nothing, which can reorder
      or hide the text around them, such as the bidirectional controls, as
      {!Console_text.sanitize} does.

    So no view can move the cursor, change the colours of what follows it, or
    show its text in another order. A line feed starts a new line, and the
    functions below work line by line.

    Widths are counted in terminal cells, as a terminal draws the text: a wide
    character, such as a Chinese one or an emoji, takes two cells, while an
    accent added to a letter takes none, even when it is in another style than
    the letter. *)

(** {1:views Views} *)

type t = View_core.t
(** The type for views. *)

val empty : t
(** [empty] is the view with no text. *)

val text : string -> t
(** [text s] is [s], unstyled. *)

val styled : Style.t -> t -> t
(** [styled base v] is [v] with [base] under each of its styles: each part of
    [v] in [style] is in [Style.(base + style)]. *)

val link : uri:string -> t -> t
(** [link ~uri v] is [v] as a link to [uri], which a terminal that supports
    links opens on a click. Printed without colours, it is the text of [v]
    alone.

    Each byte of [uri] that a URI may not hold
    ({{:https://www.rfc-editor.org/rfc/rfc3986#section-2}RFC 3986}) is written
    percent-encoded. Control characters, spaces and the bytes of non-ASCII
    characters are such bytes. The other bytes, escapes included, are written as
    they are. An empty [uri] links nowhere: the view is [v]. *)

val role : Theme.role -> t -> t
(** [role r v] is [v] styled, as by {!val-styled}, with the style of the role
    [r] in the formatter's theme ({!Theme.get}), looked up when [v] is printed.
    [role `Succeeded (text "passed")] is green in {!Theme.default} and Nord's
    green in {!Theme.nord}. A part of [v] with a colour of its own keeps it, so
    [role `Succeeded (link ~uri (text "report"))] is a link in the success
    colour.

    It is as wide as [v]. It is printed in colours at the formatter's
    {!Color.val-depth}, and as the text of [v] without colour. {!to_string} uses
    the program's theme ({!Theme.program}). *)

val gradient : ?colors:Color.t list -> t -> t
(** [gradient ~colors v] is [v] in colours graded along each of its lines, over
    its own styles. The stops are [colors] (defaults to the {!Theme.palette} of
    the formatter's theme; [[]] also means the default).

    The stops spread evenly along a line, the first on its first grapheme
    cluster and the last on its last, and each cluster takes the {!Color.blend}
    of the two stops around its position. The view is as wide as [v], printed in
    colours at the formatter's {!Color.val-depth} and as the text of [v] without
    colour. *)

val ( ++ ) : t -> t -> t
(** [s0 ++ s1] is [s0] followed by [s1]. *)

val concat : ?sep:t -> t list -> t
(** [concat ~sep views] is [views] in order, [sep] (defaults to {!empty})
    between each two. *)

val hcat : ?gap:int -> t list -> t
(** [hcat ~gap views] is [views] side by side, [gap] spaces apart (defaults to
    [2]).

    - Its line [i] is the line [i] of each view. Each view but the last is
      padded to its {!width}, and a view with fewer lines gives blank ones. A
      view with no text, such as {!empty}, takes no column and no gap, as it
      takes no line in {!vcat}.
    - Each line ends with the last view that has text on it, so [hcat] adds no
      trailing spaces.

    Without a room, as under {!width} or {!to_string}, each view in it is laid
    out at its natural size. In a room [w] wide ({!fit}), the views that do not
    grow are laid out left to right, each in the room the views before it leave;
    the views that grow, such as a {!Bar.horizontal} with no width, then share
    the rest by their weights. To make a view take less, wrap it in {!sized}.
    The result goes in a table cell, a panel, a tree or a display's view, as any
    view does:

    {[
    let report tree panel =
      Console.(View.hcat [ Tree.view tree; View.sized ~width:30 panel ])
    ]}

    @raise Invalid_argument if [gap] is negative. *)

val vcat : ?gap:int -> t list -> t
(** [vcat ~gap views] is the lines of each of [views] in turn, [gap] blank lines
    apart (defaults to [0]). A view with no text, such as {!empty} or [text ""],
    takes no line and no gap, so a part left out leaves no blank line. A blank
    line is a line feed: [text "\n" ++ v] is [v] below a blank line, and
    [concat ~sep:(text "\n") lines] is [lines] one under the other, the blank
    ones kept.

    Each of [views] is laid out in the room of the [vcat]:

    {[
    let report title table =
      Console.(View.sized ~width:60 (View.vcat [ title; table ]))
    ]}

    @raise Invalid_argument if [gap] is negative. *)

type align = [ `Left | `Center | `Right ]
(** The type for the place of a view in a room wider than it: at its left edge,
    in its middle or at its right edge, as a {!Table} column or a {!Rule} label
    sits. *)

(** {1:room Room and time}

    A view is laid out where it is printed. Its {e room} is the width and the
    height it may take: the formatter's margin under {!pp}, the terminal under a
    {!Display}, a column of a {!Table}, what the guides of a {!Tree} leave a
    label, or what {!sized} gives. Its {e time} ({!Room.time}) is the seconds
    since the display that draws it started, and [0.] when it is printed once.

    A view's size depends on its room alone, never on its time: it is measured
    at time [0.], so its frames in a display all have the same size. *)

val fit : ?min_width:int -> ?grow:int -> (Room.t -> t) -> t
(** [fit f] is the view [f room], where [room] is the {!Room} it is laid out in.
    A room holds a width and a height, each [None] where nothing bounds it (as
    under {!hcat} without a room, {!width}, {!truncate} or {!to_string}, which
    take a view at its natural size), the time it is drawn at, and the theme and
    character set of the formatter. The view [f] returns is laid out in the same
    room.

    - [grow] (defaults to [0]) is its weight in an {!hcat} laid out in a room: a
      part that grows shares what the others leave, in the ratio of the weights,
      each part taking the integer part of its share and the cells left going
      one each to the largest remainders.
    - [min_width] is the width below which it should not be laid out: a {!Table}
      does not narrow its column below it while another column can narrow
      (defaults to the [min_width] of the view [f] gives at its natural size,
      which for text is the width of its widest word).

    Its size must depend on its room alone, not on its time: every frame has the
    lines of its frame at time [0.], each cut or padded to the width it has
    there. [f] may be called several times a frame, and must be a function of
    its room and of the program's state. A display lays out a [fit] again at
    every frame, so a view of the program's own state, such as a table it
    updates, is written as a [fit]:

    {[
    let status jobs =
      Console.View.fit (fun _ ->
          Console.Table.view
            [ Console.Table.column (Console.View.text "Job") ]
            (List.map (fun j -> [ Console.View.text j ]) !jobs))
    ]}

    @raise Invalid_argument if [grow] is negative. *)

val sized : ?width:int -> ?height:int -> t -> t
(** [sized ~width ~height v] is [v] laid out in a room at most [width] cells
    wide and [height] lines tall, and within the room it is printed in. A bound
    not given is the room's. The invariant [width (sized ~width:w v) <= w] holds
    for a widget, which fits its room. *)

val place : ?align:align -> ?width:int -> ?height:int -> t -> t
(** [place ~align ~width ~height v] is [v] in a box [width] cells wide and
    [height] lines tall (each defaults to that of [v]). Each line is placed at
    [align] (defaults to [`Left]) and padded with spaces, or cut as by
    {!truncate} if wider; blank lines are added below, or the lines past
    [height] dropped. [v] is laid out in that box. A right-aligned column of a
    fixed width is a [place]:

    {[
    let status verb what =
      Console.View.(
        hcat ~gap:1 [ place ~align:`Right ~width:12 (text verb); text what ])
    ]} *)

(** {1:measuring Measuring and fitting} *)

val width : t -> int
(** [width s] is the number of cells the widest line of [s] takes at its natural
    size ({!fit}). The invariant [width (s0 ++ s1) <= width s0 + width s1]
    holds. *)

val height : t -> int
(** [height s] is the number of lines of [s] at its natural size: [0] for a view
    with no text, and otherwise one more than its line feeds, so
    [height (text "a\n")] is [2]. The invariant [height (vcat views)] is the sum
    of the [height]s of [views]. *)

val truncate : int -> t -> t
(** [truncate w s] is [s] with each line wider than [w] cells cut between two
    characters and ended with an ellipsis within [w] cells: U+2026, or three
    dots on a formatter that is not UTF-8 or in a theme drawn in ASCII, such as
    {!Theme.ascii}. Lines that fit are kept as they are, and so are the styles
    and links of what is kept. It is {!empty} if [w] is not positive. The
    invariants [width (truncate w s) <= max 0 w] and [truncate w s = s] when
    [width s <= w] hold. *)

val crop : ?left:int -> ?width:int -> t -> t
(** [crop ~left ~width v] is the cells of [v] from [left] (defaults to [0]) to
    [left + width] (defaults to the end of each line), on each line, cut with no
    ellipsis: a window onto [v] at its natural size. A wide character cut by the
    left edge leaves its cells in the window blank, and one cut by the right
    edge is left out. Each line keeps its styles and links, and is not padded.
    It is {!empty} if [width] is not positive. A marquee is a [crop]:

    {[
    let marquee ~width title =
      Console.View.fit (fun room ->
          let n = Console.View.width title + 3 in
          let left = int_of_float (Console.Room.time room *. 4.) mod n in
          let ring = Console.View.(title ++ text "   " ++ title) in
          Console.View.crop ~left ~width ring)
    ]}

    @raise Invalid_argument if [left] is negative. *)

val wrap : ?words:(string -> string list) -> int -> t -> t list
(** [wrap ~words w s] is the lines of [s], each broken into lines of at most [w]
    cells. The result is never empty.

    - A line breaks only between two of its words, given by [words l] for its
      text [l]: strings whose concatenation is [l], each ending with the spaces
      that a break after it removes. [words] defaults to cutting [l] after each
      run of spaces, so that a line breaks at a run of spaces and never after a
      slash or a hyphen: a path, a URL or a [--flag] stays whole. A cut inside a
      grapheme cluster goes to the end of the cluster.
    - The other spaces of a line are kept, its leading ones too.
    - A part drawn in the theme it is printed in, such as a {!Bar.horizontal},
      breaks only where the room ends, and keeps its spaces.
    - A word wider than [w] continues on the next lines, broken where the room
      ends.
    - A character wider than [w] cannot be drawn and is left out: a wide one
      when [w] is [1], or one with a spacing mark when [w] is its width less
      one.

    Every other non-space character of [s] is in the result, in order, with its
    styles and links. Words that cut the text only after the spaces outside a
    backquoted run keep a command whole:

    {[
    let code_words text =
      let n = String.length text in
      let words = ref [] and first = ref 0 and quoted = ref false in
      for i = 0 to n - 1 do
        if text.[i] = '`' then quoted := not !quoted
        else if
          (not !quoted) && text.[i] = ' ' && (i = n - 1 || text.[i + 1] <> ' ')
        then begin
          words := String.sub text !first (i + 1 - !first) :: !words;
          first := i + 1
        end
      done;
      if !first < n then words := String.sub text !first (n - !first) :: !words;
      List.rev !words

    let hint = Console.View.text "run `dune build @fmt` first"
    let lines = Console.View.wrap ~words:code_words 18 hint
    ]}

    [lines] are [run], [`dune build @fmt`] and [first], where the default words
    cut the command after [build].

    @raise Invalid_argument
      if [w] is not positive, or if the [words] of a line do not make it up. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a view at time [0.], in a room as wide as the formatter's
    margin ({!fit}), with its styles and links iff the formatter's style
    renderer ({!Fmt.style_renderer}) is [`Ansi_tty]. Its colours are those the
    formatter's {!Color.val-depth} shows. Each style that is turned on is turned
    off after the text it covers, and before each line feed. It is drawn in the
    formatter's theme ({!Theme.get}) and {!Fmt.utf_8}.

    A widget laid out in its room is no wider than the margin, while text is
    printed whole: {!truncate} or {!wrap} fit it to a width first. A line feed
    is printed as it is, not as a break of the enclosing box, so the box's
    indentation does not apply to the next line. Print a view of several lines
    on lines of its own, as ["%a@."] does. *)

val to_string : ?styled:bool -> t -> string
(** [to_string v] is [v] at its natural size and at time [0.], drawn in UTF-8
    with the program's theme ({!Theme.program}). Its styles and links are in
    24-bit colour iff [styled] (defaults to [false]). *)
