(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

let style n =
  let c = Color.palette (n land 255) in
  let parts =
    [
      Style.bold;
      Style.faint;
      Style.italic;
      Style.underline;
      Style.blink;
      Style.reverse;
      Style.strikethrough;
      Style.fg c;
      Style.bg c;
    ]
  in
  List.fold_left
    (fun acc (bit, s) ->
      if n land (1 lsl (bit + 8)) <> 0 then Style.(acc + s) else acc)
    Style.none
    (List.mapi (fun i s -> (i, s)) parts)

(* [none] is a unit of [+], [+] is associative, the colours of its right
   argument win, and [is_none] holds of [none] alone. *)
let test_laws a b c =
  let a = style a and b = style b and c = style c in
  check (Style.equal a Style.(a + none));
  check (Style.equal a Style.(none + a));
  check (Style.equal Style.(a + (b + c)) Style.(a + b + c));
  (match Style.foreground b with
  | Some f -> check (Style.foreground Style.(a + b) = Some f)
  | None -> check (Style.foreground Style.(a + b) = Style.foreground a));
  check (Style.is_none a = Style.equal a Style.none);
  ignore (Fmt.str "%a" Style.pp a)

let suite =
  ("style", [ test_case "composition laws" [ int; int; int ] test_laws ])
