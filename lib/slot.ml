(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a op = Get of 'a option ref | Set of 'a

type 'a t = {
  wrap : 'a op -> Format.stag;
  unwrap : Format.stag -> 'a op option;
}

let v (type a) () : a t =
  let module M = struct
    type Format.stag += Op of a op
  end in
  {
    wrap = (fun op -> M.Op op);
    unwrap = (function M.Op op -> Some op | _ -> None);
  }

(* What the tag function of a slot answers, which no other answers. *)
let ack = "\000console.slot"

let ask s ppf op =
  let funs = Format.pp_get_formatter_stag_functions ppf () in
  String.equal (funs.mark_open_stag (s.wrap op)) ack

let find s ppf =
  let r = ref None in
  ignore (ask s ppf (Get r));
  !r

let set s ppf v =
  if not (ask s ppf (Set v)) then begin
    let current = ref v in
    let funs = Format.pp_get_formatter_stag_functions ppf () in
    let other = funs.mark_open_stag in
    let mark_open_stag stag =
      match s.unwrap stag with
      | Some (Get r) ->
          r := Some !current;
          ack
      | Some (Set v) ->
          current := v;
          ack
      | None -> other stag
    in
    Format.pp_set_formatter_stag_functions ppf { funs with mark_open_stag }
  end
