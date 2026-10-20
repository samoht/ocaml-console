(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type piece =
  | Spinner
  | Label
  | Bar of int
  | Percent
  | Count
  | Rate
  | Elapsed
  | Text of string
  | Accent of string
  | Spacer

type t = piece list

let spinner = Spinner
let label = Label

let bar n =
  if n < 0 then invalid_arg "Console.Display.Line.bar: negative width";
  Bar n

let percent = Percent
let count = Count
let rate = Rate
let elapsed = Elapsed
let text s = Text (Display_text.clean s)
let accent s = Accent (Display_text.clean s)
let spacer = Spacer
let is_label = function Label -> true | _ -> false
let is_spacer = function Spacer -> true | _ -> false

let v pieces =
  (* A row has one flexible piece of each kind: the label that gives way and
     the spacer that pushes what follows to the right edge. *)
  let count is = List.length (List.filter is pieces) in
  if count is_label > 1 then
    invalid_arg "Console.Display.Line.v: more than one label";
  if count is_spacer > 1 then
    invalid_arg "Console.Display.Line.v: more than one spacer";
  pieces

let pp_piece ppf = function
  | Spinner -> Fmt.string ppf "spinner"
  | Label -> Fmt.string ppf "label"
  | Bar n -> Fmt.pf ppf "bar %d" n
  | Percent -> Fmt.string ppf "percent"
  | Count -> Fmt.string ppf "count"
  | Rate -> Fmt.string ppf "rate"
  | Elapsed -> Fmt.string ppf "elapsed"
  | Text s -> Fmt.pf ppf "text %S" s
  | Accent s -> Fmt.pf ppf "accent %S" s
  | Spacer -> Fmt.string ppf "spacer"

let pp = Fmt.Dump.list pp_piece

let default =
  v
    [
      spinner;
      text " ";
      label;
      spacer;
      bar 12;
      text " ";
      count;
      text " ";
      rate;
      text " ";
      elapsed;
    ]
