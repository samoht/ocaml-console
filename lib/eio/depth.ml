(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Color = Console.Color

let is_param c = (c >= '0' && c <= '9') || c = ';'

(* The end of the parameters of a CSI opened at [i], when they are complete. *)
let csi_end s i =
  let n = String.length s in
  let rec go j = if j < n && is_param s.[j] then go (j + 1) else j in
  if i + 1 >= n then `Cut
  else if s.[i + 1] <> '[' then `Not_csi
  else
    let j = go (i + 2) in
    if j >= n then `Cut else `End j

let code ~fg d c =
  let c = Color.downsample d c in
  if fg then Color.to_fg_code c else Color.to_bg_code c

let rec params d = function
  | (("38" | "48") as p) :: "5" :: n :: rest -> (
      match int_of_string_opt n with
      | Some n when n >= 0 && n <= 255 ->
          code ~fg:(p = "38") d (Color.palette n) :: params d rest
      | _ -> p :: "5" :: params d (n :: rest))
  | (("38" | "48") as p) :: "2" :: r :: g :: b :: rest -> (
      match List.map int_of_string_opt [ r; g; b ] with
      | [ Some r; Some g; Some b ]
        when List.for_all (fun v -> v <= 255) [ r; g; b ] ->
          code ~fg:(p = "38") d (Color.rgb r g b) :: params d rest
      | _ -> p :: "2" :: params d (r :: g :: b :: rest))
  | p :: rest -> p :: params d rest
  | [] -> []

let rewrite d ps = String.concat ";" (params d (String.split_on_char ';' ps))

(* [split d s] is the rewritten part of [s] and the suffix that opens a
   sequence [s] ends before completing. *)
let split d s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    match String.index_from_opt s i '\027' with
    | None ->
        Buffer.add_substring b s i (n - i);
        ""
    | Some e -> (
        Buffer.add_substring b s i (e - i);
        match csi_end s e with
        | `Cut -> String.sub s e (n - e)
        | `Not_csi ->
            Buffer.add_char b '\027';
            go (e + 1)
        | `End j ->
            let ps = String.sub s (e + 2) (j - e - 2) in
            Buffer.add_string b "\027[";
            Buffer.add_string b (if s.[j] = 'm' then rewrite d ps else ps);
            go j)
  in
  let rest = go 0 in
  (Buffer.contents b, rest)

let sgr d s =
  let out, rest = split d s in
  out ^ rest

let installed : Format.formatter list Atomic.t = Atomic.make []

let install ppf d =
  if d <> `True_color && not (List.memq ppf (Atomic.get installed)) then begin
    Atomic.set installed (ppf :: Atomic.get installed);
    let funs = Format.pp_get_formatter_out_functions ppf () in
    let held = ref "" in
    let out_string s pos len =
      let out, rest = split d (!held ^ String.sub s pos len) in
      held := rest;
      funs.out_string out 0 (String.length out)
    in
    let out_flush () =
      let h = !held in
      held := "";
      funs.out_string h 0 (String.length h);
      funs.out_flush ()
    in
    Format.pp_set_formatter_out_functions ppf
      { funs with out_string; out_flush }
  end
