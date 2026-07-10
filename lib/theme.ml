(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A theme bundles every visual choice the widgets share -- the accent colour
   and palette, the box-drawing border, the tree guide, and the live display's
   spinner, bar style and markers -- so one value styles tables, trees, panels
   and the progress display consistently. It is pure data; the widgets render
   with it. *)

type color = Ansi.color

(* Amber-phosphor monochrome, like an old DOS satellite-workstation CRT. *)
let amber : color = `Hex (0xff, 0xb0, 0x00)
let matrix_green : color = `Hex (0x33, 0xff, 0x33)

(* Tree-guide glyphs. *)
type guide = { branch : string; last : string; pipe : string; space : string }

let ascii_guide =
  { branch = "+-- "; last = "+-- "; pipe = "|   "; space = "    " }

let unicode_guide =
  { branch = "├── "; last = "└── "; pipe = "│   "; space = "    " }

type bar = [ `Blocky | `Smooth | `Rainbow ]

type t = {
  accent : color;
  palette : color list;
  border : Border.t;
  animated_border : bool;
  guide : guide;
  spinner : Spinner.t;
  bar : bar;
  done_marker : string;
  ok_marker : string;
  fail_marker : string;
}

let equal a b =
  a.accent = b.accent && a.palette = b.palette
  && Border.equal a.border b.border
  && Bool.equal a.animated_border b.animated_border
  && a.guide = b.guide
  && Spinner.equal a.spinner b.spinner
  && a.bar = b.bar
  && String.equal a.done_marker b.done_marker
  && String.equal a.ok_marker b.ok_marker
  && String.equal a.fail_marker b.fail_marker

let pp ppf t =
  Fmt.pf ppf "{ done = %s; ok = %s; fail = %s }" t.done_marker t.ok_marker
    t.fail_marker

let v ?(accent = `Cyan) ?(palette = []) ?(border = Border.rounded)
    ?(animated_border = false) ?(guide = unicode_guide) ?(bar = `Smooth)
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
    guide = ascii_guide;
    spinner = Spinner.ascii;
    bar = `Blocky;
    done_marker = "[+]";
    ok_marker = "[OK]";
    fail_marker = "[FAIL]";
  }

(* Modern unicode: rounded border, braille spinner, smooth bar, a check mark,
   and a green tick / red cross result banner. *)
let unicode =
  {
    accent = `Cyan;
    palette = [ `Cyan; `Magenta; `Green; `Yellow; `Blue ];
    border = Border.rounded;
    animated_border = false;
    guide = unicode_guide;
    spinner = Spinner.braille;
    bar = `Smooth;
    done_marker = "✔";
    ok_marker = "✔";
    fail_marker = "✘";
  }

(* The Matrix: monochrome green on black, single-line border, braille
   spinner. *)
let matrix =
  {
    accent = matrix_green;
    palette = [ matrix_green ];
    border = Border.single;
    animated_border = true;
    guide = unicode_guide;
    spinner = Spinner.braille;
    bar = `Smooth;
    done_marker = "✓";
    ok_marker = "✓";
    fail_marker = "✗";
  }

(* Rainbow: rows and nodes cycle the spectrum, and the bar is a gradient. *)
let rainbow =
  {
    accent = `Magenta;
    palette = [ `Red; `Yellow; `Green; `Cyan; `Blue; `Magenta ];
    border = Border.rounded;
    animated_border = false;
    guide = unicode_guide;
    spinner = Spinner.braille;
    bar = `Rainbow;
    done_marker = "✔";
    ok_marker = "✔";
    fail_marker = "✘";
  }

let default = dos
