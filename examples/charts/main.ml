(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Vertical bars side by side: a sparkline of a week's load on one row, and
   the same load as a histogram five rows tall, its days under it. *)

open Console

let days = [ "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"; "Sun" ]
let load = [ 0.12; 0.35; 0.81; 0.55; 0.93; 0.41; 0.07 ]

let () =
  Console_tty.setup ();
  let bars gap = View.hcat ~gap (List.map Bar.vertical load) in
  let label day = View.place ~align:`Center ~width:3 (View.text day) in
  let histogram =
    View.hcat ~gap:1
      (List.map2
         (fun day frac ->
           View.vcat
             [
               View.place ~align:`Center ~width:3
                 (View.sized ~height:5 (Bar.vertical frac));
               View.role `Muted (label day);
             ])
         days load)
  in
  Fmt.pr "%a  %a@.@.%a@." View.pp
    (View.text "load this week")
    View.pp (bars 0) View.pp histogram
