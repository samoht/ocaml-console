(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Views: styled text, and everything built from it.

    A view is what console prints. It is drawn in a room, at a time. The [view]
    of a widget, such as {!Table.view}, is a view, and views compose: a table
    can go in a panel, the panel in a cell of another table, and that table next
    to a tree ({!hcat}) or above it ({!vcat}). A {!Display} draws a view at
    every frame, in a room the size of the terminal. Printing a view ({!pp},
    {!to_string}) draws it once, at time [0]. Most views look the same at every
    time. A {!Spinner.view} changes over time, and so do the frames of a theme
    with motion ({!Theme.val-motion}).

    A view holds only text that can be printed. The constructors:
    - replace each control character with a space;
    - replace each line end (as {!Console_text.sanitize} defines it) with a line
      feed;
    - replace each malformed UTF-8 sequence with U+FFFD;
    - remove the characters that a terminal draws as nothing, such as the
      bidirectional controls, as {!Console_text.sanitize} does. These characters
      can reorder or hide the text around them.

    So a view cannot move the cursor, change the colours of the text after it,
    or show its text in another order. A line feed starts a new line, and the
    functions below work line by line.

    Widths are counted in terminal cells, the way a terminal draws the text. A
    wide character, such as a Chinese character or an emoji, takes two cells. An
    accent added to a letter takes no cell, even when its style differs from the
    style of the letter. *)

(** {1:views Views} *)

type t = View_core.t
(** The type for views. *)

val empty : t
(** [empty] is the view with no text. *)

val text : string -> t
(** [text s] is [s], unstyled. *)

val styled : Style.t -> t -> t
(** [styled base v] is [v] with [base] added under each of its styles: a part of
    [v] in style [style] is drawn in [Style.(base + style)]. *)

val link : uri:string -> t -> t
(** [link ~uri v] is [v] as a link to [uri]. A terminal that supports links
    opens [uri] on a click. Printed without colours, it is only the text of [v].

    Each byte of [uri] that is not allowed in a URI
    ({{:https://www.rfc-editor.org/rfc/rfc3986#section-2}RFC 3986}) is
    percent-encoded. Control characters, spaces and the bytes of non-ASCII
    characters are such bytes. The other bytes, escapes included, are written
    unchanged. If [uri] is empty, the view is [v], with no link. *)

val role : Theme.role -> t -> t
(** [role r v] is [v] styled, as by {!val-styled}, with the style of role [r] in
    the formatter's theme ({!Theme.get}). The style is looked up when [v] is
    printed. For example, [role `Succeeded (text "passed")] is green in
    {!Theme.default} and Nord's green in {!Theme.nord}. A part of [v] with its
    own colour keeps it, so [role `Succeeded (link ~uri (text "report"))] is a
    link in the success colour.

    It is as wide as [v]. With colours, it uses the formatter's
    {!Color.val-depth}. Without colours, it is the text of [v]. {!to_string}
    uses the program's theme ({!Theme.program}). *)

val gradient : ?colors:Color.t list -> t -> t
(** [gradient ~colors v] is [v] with a colour gradient along each of its lines,
    drawn over the styles of [v]. The colour stops are [colors] (defaults to the
    {!Theme.palette} of the formatter's theme; [[]] also gives the default).

    The stops are spread evenly along a line: the first stop is on the first
    grapheme cluster and the last stop on the last cluster. Each cluster gets
    the {!Color.blend} of the two stops on either side of its position. The view
    is as wide as [v]. It is printed in colours at the formatter's
    {!Color.val-depth}, or as the text of [v] without colours. *)

val ( ++ ) : t -> t -> t
(** [s0 ++ s1] is [s0] followed by [s1]. *)

val concat : ?sep:t -> t list -> t
(** [concat ~sep views] is [views] in order, with [sep] (defaults to {!empty})
    between each two. *)

val hcat : ?gap:int -> t list -> t
(** [hcat ~gap views] is [views] side by side, [gap] spaces apart (defaults to
    [2]).

    - Line [i] of the result joins line [i] of each view. Each view except the
      last is padded to its {!width}. A view with fewer lines gives blank lines.
      A view with no text, such as {!empty}, takes no column and no gap, as it
      takes no line in {!vcat}.
    - Each line ends with the last view that has text on that line, so [hcat]
      adds no trailing spaces.

    Without a room, as under {!width} or {!to_string}, each view is laid out at
    its natural size. In a room [w] cells wide ({!fit}), the views that do not
    grow are laid out first, from left to right. Each one gets the room that the
    views before it leave. Then the views that grow, such as a {!Bar.horizontal}
    with no width, share the rest by their weights. To make a view take less
    room, wrap it in {!sized}. Like any view, the result can go in a table cell,
    a panel, a tree or a display:

    {[
    let report tree panel =
      Console.(View.hcat [ Tree.view tree; View.sized ~width:30 panel ])
    ]}

    @raise Invalid_argument if [gap] is negative. *)

val vcat : ?gap:int -> t list -> t
(** [vcat ~gap views] is the lines of each view of [views], one view after the
    other, with [gap] blank lines between them (defaults to [0]). A view with no
    text, such as {!empty} or [text ""], takes no line and no gap, so a part
    left out leaves no blank line. A blank line is a line feed: [text "\n" ++ v]
    is [v] below a blank line, and [concat ~sep:(text "\n") lines] puts [lines]
    one under the other and keeps the blank ones.

    Each view of [views] is laid out in the room of the [vcat]:

    {[
    let report title table =
      Console.(View.sized ~width:60 (View.vcat [ title; table ]))
    ]}

    @raise Invalid_argument if [gap] is negative. *)

type align = [ `Left | `Center | `Right ]
(** The type for the position of a view in a room wider than the view: at the
    left edge, in the middle or at the right edge. {!Table} columns and {!Rule}
    labels are aligned this way. *)

(** {1:room Room and time}

    A view is laid out where it is printed. Its {e room} is the width and the
    height it may take: the formatter's margin under {!pp}, the terminal under a
    {!Display}, a {!Table} column, the space that the guides of a {!Tree} leave
    for a label, or the bounds that {!sized} gives. Its {e time} ({!Room.time})
    is the number of seconds since the display that draws it started, or [0.]
    when it is printed once.

    The size of a view depends only on its room, never on its time. The size is
    measured at time [0.], so all the frames of a view in a display have the
    same size. *)

val fit : ?min_width:int -> ?grow:int -> (Room.t -> t) -> t
(** [fit f] is the view [f room], where [room] is the {!Room} that the view is
    laid out in. A room holds:
    - a width and a height, each [None] when nothing bounds it, as under {!hcat}
      without a room, {!width}, {!truncate} or {!to_string}, which lay a view
      out at its natural size;
    - the time it is drawn at;
    - the theme and the character set of the formatter.

    The view that [f] returns is laid out in the same room.

    - [grow] (defaults to [0]) is the weight of the view in an {!hcat} laid out
      in a room. The parts that grow share the room that the other parts leave,
      in proportion to their weights. Each part takes the integer part of its
      share, and the cells left go one each to the parts with the largest
      remainders.
    - [min_width] is the width below which the view should not be laid out. A
      {!Table} does not narrow its column below it while another column can
      still narrow. It defaults to the [min_width] of the view that [f] returns
      at its natural size. For text, that is the width of its widest word.

    The size of the view must depend only on its room, not on its time. Every
    frame has as many lines as the frame at time [0.], and each line is cut or
    padded to the width of the same line at time [0.]. [f] may be called several
    times per frame, and its result must depend only on its room and on the
    program's state. A display lays out a [fit] again at every frame, so write a
    view of the program's own state, such as a table that the program updates,
    as a [fit]:

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
    wide and [height] lines tall, and no larger than the room it is printed in.
    A bound that is not given is the bound of that room, and a negative one is
    [0]. A widget fits its room, so for a widget
    [width (sized ~width:w v) <= max 0 w]. *)

val place : ?align:align -> ?width:int -> ?height:int -> t -> t
(** [place ~align ~width ~height v] is [v] in a box [width] cells wide and
    [height] lines tall (each defaults to the size of [v], and is [0] if
    negative). Each line is placed at [align] (defaults to [`Left]) and padded
    with spaces. A line wider than the box is cut as by {!truncate}. Blank lines
    are added below, or the lines after [height] are dropped. [v] is laid out in
    that box. For example, a right-aligned column of fixed width is a [place]:

    {[
    let status verb what =
      Console.View.(
        hcat ~gap:1 [ place ~align:`Right ~width:12 (text verb); text what ])
    ]} *)

(** {1:measuring Measuring and fitting} *)

val width : t -> int
(** [width s] is the number of cells of the widest line of [s] at its natural
    size ({!fit}). [width (s0 ++ s1) <= width s0 + width s1]. *)

val height : t -> int
(** [height s] is the number of lines of [s] at its natural size: [0] for a view
    with no text, otherwise the number of its line feeds plus one. So
    [height (text "a\n")] is [2]. [height (vcat views)] is the sum of the
    [height]s of [views]. *)

val truncate : int -> t -> t
(** [truncate w s] is [s] with each line wider than [w] cells cut between two
    characters and ended with an ellipsis, all within [w] cells. The ellipsis is
    U+2026, or three dots when the formatter is not UTF-8 or the theme is drawn
    in ASCII, such as {!Theme.ascii}. Lines that fit are unchanged. The text
    that is kept keeps its styles and links. It is {!empty} if [w] is not
    positive. [width (truncate w s) <= max 0 w], and [truncate w s = s] when
    [width s <= w]. *)

val crop : ?left:int -> ?width:int -> t -> t
(** [crop ~left ~width v] is a window onto [v] at its natural size: on each
    line, the cells from [left] (defaults to [0]) to [left + width] (defaults to
    the end of the line), cut with no ellipsis. When the left edge cuts a wide
    character, its cells inside the window are blank. When the right edge cuts
    one, it is left out. Each line keeps its styles and links, and is not
    padded. It is {!empty} if [width] is not positive. A marquee is a [crop]:

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

    - A line breaks only between two of its words. For a line with text [l],
      [words l] gives its words: strings that concatenate to [l], each ending
      with the spaces that a break after it removes. By default, [words] cuts
      [l] after each run of spaces. So a line breaks at a run of spaces and
      never after a slash or a hyphen: a path, a URL or a [--flag] stays whole.
      A cut inside a grapheme cluster moves to the end of the cluster.
    - The other spaces of a line are kept, including its leading spaces.
    - A part drawn in the theme it is printed in, such as a {!Bar.horizontal},
      breaks only where the room ends, and keeps its spaces.
    - A word wider than [w] continues on the next lines, broken where the room
      ends.
    - A character wider than [w] cannot be drawn and is left out: a wide
      character when [w] is [1], or a character with a spacing mark when [w] is
      its width minus one.

    Every other non-space character of [s] is in the result, in order, with its
    styles and links. The [words] below cut the text after spaces only outside
    backquotes, so a quoted command stays whole:

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

    [lines] are [run], [`dune build @fmt`] and [first]. The default words would
    cut the command after [build].

    @raise Invalid_argument
      if [w] is not positive, or if the [words] of a line do not concatenate to
      the line. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a view at time [0.], in a room as wide as the formatter's
    margin ({!fit}). It prints styles and links iff the formatter's style
    renderer ({!Fmt.style_renderer}) is [`Ansi_tty], with the colours that the
    formatter's {!Color.val-depth} can show. Each style is turned off after the
    text it covers, and before each line feed. The view is drawn in the
    formatter's theme ({!Theme.get}) and with its {!Fmt.utf_8} setting.

    A widget fits its room, so it is no wider than the margin. Text is printed
    whole: use {!truncate} or {!wrap} to fit it to a width first. A line feed is
    printed as is, not as a break of the enclosing box, so the indentation of
    the box does not apply to the next line. Print a view of several lines on
    lines of its own, as ["%a@."] does. *)

val to_string : ?styled:bool -> t -> string
(** [to_string v] is [v] at its natural size and at time [0.], drawn in UTF-8
    with the program's theme ({!Theme.program}). Its styles and links are
    included, in 24-bit colour, iff [styled] (defaults to [false]). *)
