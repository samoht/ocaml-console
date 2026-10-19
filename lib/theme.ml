(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type mark =
  [ `Running | `Succeeded | `Failed | `Cancelled | `Info | `Warning | `Error ]

type t = {
  accent : Color.t;
  palette : Color.t list;
  ok : Color.t;
  warn : Color.t;
  fail : Color.t;
  header : Style.t;
  background : Color.t option;
  border : Border.t;
  guide : Guide.t;
  spinner : Spinner.t;
  marks : mark -> string;
}

let accent t = t.accent
let palette t = t.palette
let ok t = t.ok
let warn t = t.warn
let fail t = t.fail
let header t = t.header
let background t = t.background
let border t = t.border
let guide t = t.guide
let spinner t = t.spinner

(* A mark is drawn on one line, as text. *)
let mark t m = Console_text.sanitize ~keep_newlines:false (t.marks m)

let unicode_marks = function
  | `Running -> "\xe2\x80\xa2"
  | `Succeeded -> "\xe2\x9c\x93"
  | `Failed | `Error -> "\xe2\x9c\x97"
  | `Cancelled -> "\xe2\x97\x8b"
  | `Info -> "\xe2\x80\xba"
  | `Warning -> "!"

let ascii_marks = function
  | `Running -> "[-]"
  | `Succeeded -> "[+]"
  | `Failed -> "[x]"
  | `Cancelled -> "[~]"
  | `Info -> "#"
  | `Warning -> "!"
  | `Error -> "x"

let unicode =
  {
    accent = Color.cyan;
    palette = [ Color.cyan ];
    ok = Color.green;
    warn = Color.yellow;
    fail = Color.red;
    header = Style.bold;
    background = None;
    border = Border.rounded;
    guide = Guide.unicode;
    spinner = Spinner.braille;
    marks = unicode_marks;
  }

let v ?(accent = unicode.accent) ?palette ?(ok = unicode.ok)
    ?(warn = unicode.warn) ?(fail = unicode.fail) ?(header = unicode.header)
    ?background ?(border = unicode.border) ?(guide = unicode.guide)
    ?(spinner = unicode.spinner) ?(marks = unicode.marks) () =
  let palette =
    match palette with None | Some [] -> [ accent ] | Some p -> p
  in
  {
    accent;
    palette;
    ok;
    warn;
    fail;
    header;
    background;
    border;
    guide;
    spinner;
    marks;
  }

let amber = Color.rgb 0xff 0xb0 0x00

let dos =
  v ~accent:amber ~ok:amber ~warn:amber ~fail:(Color.rgb 0xff 0x55 0x55)
    ~background:Color.black ~border:Border.ascii ~guide:Guide.ascii
    ~spinner:Spinner.ascii ~marks:ascii_marks ()

let matrix_green = Color.rgb 0x33 0xff 0x33

let matrix =
  v ~accent:matrix_green ~ok:matrix_green ~warn:(Color.rgb 0xcc 0xff 0x66)
    ~fail:(Color.rgb 0xff 0x33 0x33) ~background:Color.black
    ~border:Border.single ()

let rainbow =
  v ~accent:Color.magenta
    ~palette:
      [
        Color.red;
        Color.yellow;
        Color.green;
        Color.cyan;
        Color.blue;
        Color.magenta;
      ]
    ()

let pp ppf t =
  Fmt.pf ppf "@[<1>(theme@ accent=%a@ ok=%a@ warn=%a@ fail=%a@ marks=%s)@]"
    Color.pp t.accent Color.pp t.ok Color.pp t.warn Color.pp t.fail
    (String.concat " "
       (List.map (mark t)
          [ `Running; `Succeeded; `Failed; `Cancelled; `Info; `Warning; `Error ]))
