(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Ansi
open Display_state

(* A session with no task yet is still pending until it closes. *)
let complete t =
  t.torn_down || (t.rows <> [] && t.finished >= List.length t.rows)
let any_error t = List.exists (fun r -> r.routcome = Some `Failed) t.rows
let any_cancelled t = List.exists (fun r -> r.routcome = Some `Cancelled) t.rows

let marker t =
  if not (complete t) then ("[-]", t.default_style.Display_line.color)
  else if
    any_error t
    ||
    match t.pending with
    | Some (`Fail _, _) -> true
    | Some (`Ok _, _) | None -> false
  then ("[x]", Theme.fail t.theme)
  else if any_cancelled t then ("[~]", Theme.warn t.theme)
  else ("[+]", Theme.ok t.theme)

let header t ~fit =
  match t.header with
  | None -> None
  | Some h ->
      let marker, color = marker t in
      let meta =
        Fmt.str "%s (%d/%d)"
          (Display_text.span (t.now () -. t.dstart))
          t.finished (List.length t.rows)
      in
      let h =
        if not fit then h
        else
          Width.shorten
            (t.width - Width.string_width marker - Width.string_width meta - 2)
            h
      in
      let line =
        String.concat ""
          [
            code t (color_code t color);
            marker;
            code t reset_code;
            " ";
            code t bold;
            h;
            code t reset_code;
            " ";
            dimmed t meta;
          ]
      in
      Some (if fit then Width.truncate t.width line else line)

let verdict t result =
  let marker, accent, msg =
    match result with
    | `Ok msg -> (Theme.ok_marker t.theme, color_code t (Theme.ok t.theme), msg)
    | `Fail msg ->
        (Theme.fail_marker t.theme, color_code t (Theme.fail t.theme), msg)
  in
  if t.plain then styled t (dim ^ accent) marker ^ " " ^ styled t bold msg
  else begin
    let indent = Width.string_width marker + 1 in
    let body = Width.wrap ~indent t.width msg in
    (* [wrap] indents every line, including the first; the marker takes the
       first line's indent. *)
    let rest =
      if String.length body >= indent then
        String.sub body indent (String.length body - indent)
      else body
    in
    styled t (dim ^ accent) marker ^ " " ^ styled t bold rest
  end

let detail t = function
  | None -> []
  | Some d ->
      List.concat_map
        (fun l ->
          if t.plain then [ dimmed t l ]
          else
            String.split_on_char '\n' (Width.wrap t.width l)
            |> List.map (dimmed t))
        (String.split_on_char '\n' d)
