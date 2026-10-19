(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  accent : Color.t;
  palette : Color.t list;
  border : Border.t;
  guide : Guide.t;
  spinner : Spinner.t;
  done_marker : string;
  ok_marker : string;
  fail_marker : string;
}

let pp ppf t =
  Fmt.pf ppf "{ done = %s; ok = %s; fail = %s }" t.done_marker t.ok_marker
    t.fail_marker

let accent t = t.accent
let palette t = t.palette
let border t = t.border
let guide t = t.guide
let spinner t = t.spinner
let ok _ = Color.green
let fail _ = Color.red
let warn _ = Color.yellow
let done_marker t = t.done_marker
let ok_marker t = t.ok_marker
let fail_marker t = t.fail_marker
let note_marker _ = "#"
let warn_marker _ = "!"
let error_marker _ = "x"

let committed_marker _ = function
  | `Succeeded -> "=>"
  | `Failed -> "!!"
  | `Cancelled -> "--"

let unicode =
  {
    accent = Color.cyan;
    palette = [ Color.cyan ];
    border = Border.rounded;
    guide = Guide.unicode;
    spinner = Spinner.braille;
    done_marker = "✓";
    ok_marker = "✓";
    fail_marker = "✗";
  }

let v ?(accent = unicode.accent) ?palette ?(border = unicode.border)
    ?(guide = unicode.guide) ?(spinner = unicode.spinner) () =
  let palette = Option.value palette ~default:[ accent ] in
  { unicode with accent; palette; border; guide; spinner }

let amber = Color.rgb 0xff 0xb0 0x00

let dos =
  {
    accent = amber;
    palette = [ amber ];
    border = Border.ascii;
    guide = Guide.ascii;
    spinner = Spinner.ascii;
    done_marker = "[+]";
    ok_marker = "[OK]";
    fail_marker = "[FAIL]";
  }

let matrix_green = Color.rgb 0x33 0xff 0x33

let matrix =
  {
    unicode with
    accent = matrix_green;
    palette = [ matrix_green ];
    border = Border.single;
  }

let rainbow =
  {
    unicode with
    accent = Color.magenta;
    palette =
      [
        Color.red;
        Color.yellow;
        Color.green;
        Color.cyan;
        Color.blue;
        Color.magenta;
      ];
    done_marker = "✔";
    ok_marker = "✔";
    fail_marker = "✘";
  }
