(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A theme bundles every visual choice the widgets share -- the accent colour
   and palette, the box-drawing border, the tree guide, and the live display's
   spinner, bar style and markers -- so one value styles tables, trees, panels
   and the progress display consistently. It is pure data; the widgets render
   with it. *)

(* Amber-phosphor monochrome, like an old DOS satellite-workstation CRT. *)
let amber = Color.rgb 0xff 0xb0 0x00
let matrix_green = Color.rgb 0x33 0xff 0x33

type bar = [ `Blocky | `Smooth | `Rainbow ]

type t = {
  accent : Color.t;
  palette : Color.t list;
  border : Border.t;
  animated_border : bool;
  guide : Guide.t;
  spinner : Spinner.t;
  bar : bar;
  done_marker : string;
  ok_marker : string;
  fail_marker : string;
}

let equal a b =
  Color.equal a.accent b.accent
  && List.equal Color.equal a.palette b.palette
  && Border.equal a.border b.border
  && Bool.equal a.animated_border b.animated_border
  && Guide.equal a.guide b.guide
  && Spinner.equal a.spinner b.spinner
  && a.bar = b.bar
  && String.equal a.done_marker b.done_marker
  && String.equal a.ok_marker b.ok_marker
  && String.equal a.fail_marker b.fail_marker

let pp ppf t =
  Fmt.pf ppf "{ done = %s; ok = %s; fail = %s }" t.done_marker t.ok_marker
    t.fail_marker

let accent t = t.accent
let palette t = t.palette
let border t = t.border
let animated_border t = t.animated_border
let guide t = t.guide
let spinner t = t.spinner
let bar t = t.bar
let done_marker t = t.done_marker
let ok_marker t = t.ok_marker
let fail_marker t = t.fail_marker

let v ?(accent = Color.cyan) ?(palette = []) ?(border = Border.rounded)
    ?(animated_border = false) ?(guide = Guide.unicode) ?(bar = `Smooth)
    ~spinner ~done_marker () =
  {
    accent;
    palette;
    border;
    animated_border;
    guide;
    spinner;
    bar;
    done_marker;
    ok_marker = "[OK]";
    fail_marker = "[FAIL]";
  }

let with_verdict ?ok ?fail t =
  {
    t with
    ok_marker = Option.value ok ~default:t.ok_marker;
    fail_marker = Option.value fail ~default:t.fail_marker;
  }

(* Amber DOS satellite-workstation CRT: ASCII everything, blocky bar, [+] done
   marker, [OK]/[FAIL] result banner. *)
let dos =
  {
    accent = amber;
    palette = [ amber ];
    border = Border.ascii;
    animated_border = false;
    guide = Guide.ascii;
    spinner = Spinner.ascii;
    bar = `Blocky;
    done_marker = "[+]";
    ok_marker = "[OK]";
    fail_marker = "[FAIL]";
  }

(* Modern unicode: one restrained activity accent, rounded geometry, a braille
   spinner and semantic success/failure markers.  Colour identifies state; it
   does not decorate whole rows. *)
let unicode =
  {
    accent = Color.cyan;
    palette = [ Color.cyan ];
    border = Border.rounded;
    animated_border = false;
    guide = Guide.unicode;
    spinner = Spinner.braille;
    bar = `Smooth;
    done_marker = "✓";
    ok_marker = "✓";
    fail_marker = "✗";
  }

(* The Matrix: monochrome green on black, single-line border, braille
   spinner. *)
let matrix =
  {
    accent = matrix_green;
    palette = [ matrix_green ];
    border = Border.single;
    animated_border = true;
    guide = Guide.unicode;
    spinner = Spinner.braille;
    bar = `Smooth;
    done_marker = "✓";
    ok_marker = "✓";
    fail_marker = "✗";
  }

(* Rainbow: rows and nodes cycle the spectrum, and the bar is a gradient. *)
let rainbow =
  {
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
    border = Border.rounded;
    animated_border = false;
    guide = Guide.unicode;
    spinner = Spinner.braille;
    bar = `Rainbow;
    done_marker = "✔";
    ok_marker = "✔";
    fail_marker = "✘";
  }

let default = unicode
