(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A top of four cores: a box for each, side by side, above a gauge of all
   their jobs. A pool of four domains runs the jobs, and each job reports to
   its core's task and to the gauge's straight from its domain. The display
   does the rest: there is no lock, no atomic and no redraw here. *)

open Console

let cores = 4
let ticks = 90
let tick = 0.1

(* The jobs a core runs at a tick: each core's load changes every second and a
   half, by a pattern of its own. *)
let jobs core tick = Hashtbl.hash (core, tick / 15) mod 5

(* The most jobs a core runs in a second, which is a load of 100%. *)
let capacity = 4. /. tick

(* A job is real work for its domain: a chain of digests. *)
let run_job core tick j =
  let rec chain n d = if n = 0 then d else chain (n - 1) (Digest.string d) in
  let d = chain 5_000 (Fmt.str "%d.%d.%d" core tick j) in
  String.sub (Digest.to_hex d) 0 8

let sum f n = List.fold_left ( + ) 0 (List.init n f)

(* What a core does at a tick, in a domain of the pool. *)
let step ~total core task tick =
  for j = 1 to jobs core tick do
    let digest = run_job core tick j in
    Fmt.kstr (Task.log task) "job %d.%d %s" tick j digest;
    Task.advance task;
    Task.advance total
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
  let last = List.filteri (fun i _ -> i >= List.length lines - 3) lines in
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
  Eio.Switch.run @@ fun sw ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  let pool =
    Eio.Executor_pool.create ~sw ~domain_count:cores (Eio.Stdenv.domain_mgr env)
  in
  Console_eio.run ~clock ~keep_ended:true ~view @@ fun root ->
  let total = Task.start root "all cores" in
  Task.set_items total ~count:0 ~total:(sum (fun c -> sum (jobs c) ticks) cores);
  let tasks =
    List.init cores (fun c ->
        let task = Task.start root (Fmt.str "cpu%d" c) in
        Task.set_items task ~count:0 ~total:(sum (jobs c) ticks);
        (c, task))
  in
  (* At each tick the cores run their jobs side by side, and the tick ends
     once they all have. *)
  for t = 0 to ticks - 1 do
    Eio.Fiber.List.iter
      (fun (c, task) ->
        Eio.Executor_pool.submit_exn pool ~weight:1. (fun () ->
            step ~total c task t))
      tasks;
    Eio.Time.Mono.sleep clock tick
  done
