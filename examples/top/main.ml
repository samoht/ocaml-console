(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A top of four cores. Their boxes stand side by side above a gauge of all
   their jobs. Each core is a domain of its own, which at every tick runs
   its jobs and counts them on its box and on the gauge. The display does
   the rest, so the program has no synchronisation of its own. *)

open Console

let cores = 4
let ticks = 90
let tick = 0.1

(* The jobs a core runs at a tick: each core's load changes every second and a
   half, by a pattern of its own. *)
let jobs core tick = Hashtbl.hash (core, tick / 15) mod 5

(* The most jobs a core runs in a second, which is a load of 100%. *)
let capacity = 4. /. tick

(* Each job is real work for its domain, a chain of digests. *)
let run_job core tick j =
  let rec chain n d = if n = 0 then d else chain (n - 1) (Digest.string d) in
  let d = chain 5_000 (Fmt.str "%d.%d.%d" core tick j) in
  String.sub (Digest.to_hex d) 0 8

let sum f n = List.fold_left ( + ) 0 (List.init n f)

(* A core runs its jobs at each tick, in a domain of its own. *)
let core ~clock ~total c task =
  for t = 0 to ticks - 1 do
    Eio.Time.Mono.sleep clock tick;
    for j = 1 to jobs c t do
      let digest = run_job c t j in
      Fmt.kstr (Task.log task) "job %d.%d %s" t j digest;
      Task.advance task;
      Task.advance total
    done
  done

(* A core's box: its load, its jobs and the last jobs it ran. *)
let box ~width task =
  let load =
    Float.min 1. (Option.value ~default:0. (Task.rate task) /. capacity)
  in
  let count =
    match Task.progress task with
    | `Items (n, total) -> Fmt.str "%d/%d jobs" n total
    | _ -> ""
  in
  let lines = Task.output task in
  let skip = List.length lines - 3 in
  let last = List.filteri (fun i _ -> i >= skip) lines in
  View.sized ~width
    (Panel.view ~expand:true
       ~title:(View.text (Task.label task))
       (View.vcat
          [
            View.hcat ~gap:1
              [
                Bar.horizontal ~brackets:true load;
                Fmt.kstr View.text "%3.0f%%" (100. *. load);
              ];
            View.role `Muted (View.text count);
            View.place ~height:3 (View.vcat (List.map View.text last));
          ]))

(* The boxes share the width, two cells apart, above the gauge's row. The
   gauge's task is the first that starts. *)
let view root =
  View.fit @@ fun room ->
  match Task.children root with
  | [] -> View.empty
  | total :: cpus ->
      let w = Option.value ~default:100 (Room.width room) in
      let width = (w - (2 * (cores - 1))) / cores in
      View.vcat [ View.hcat (List.map (box ~width) cpus); Progress.row total ]

let () =
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  let domains = Console_eio.domain_mgr (Eio.Stdenv.domain_mgr env) in
  Console_eio.run ~clock ~keep_ended:true ~view @@ fun root ->
  let total = Task.start root "all cores" in
  Task.set_items total ~count:0 ~total:(sum (fun c -> sum (jobs c) ticks) cores);
  let cpus =
    List.init cores (fun c ->
        let task = Task.start root (Fmt.str "cpu%d" c) in
        Task.set_items task ~count:0 ~total:(sum (jobs c) ticks);
        (c, task))
  in
  Eio.Fiber.List.iter
    (fun (c, task) ->
      Eio.Domain_manager.run domains (fun () -> core ~clock ~total c task))
    cpus
