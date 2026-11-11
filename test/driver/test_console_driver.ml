(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

(* What a scheduler with one fiber provides: everything a backend must write,
   without Eio and without effects. *)
module Driver = Console_driver.Make (struct
  let clock () () = 0.
  let tick _ = ()

  type 'a key = 'a option ref

  let key () = ref None
  let get k = !k

  let with_binding k v f =
    let saved = !k in
    k := Some v;
    Fun.protect ~finally:(fun () -> k := saved) f

  type wake = unit

  let wake () = ()
  let notify () = ()

  let rec wait () f =
    match f () with
    | Some v -> v
    | None ->
        Domain.cpu_relax ();
        wait () f

  let in_fiber () = true
  let background _ f = f ()
  let first a _ = a ()
  let protect f = f ()
  let check () = ()
  let cancelled _ = false
  let write_error = function Sys_error _ -> true | _ -> false
  let sleep _ = ()
end)

let buffer () =
  let b = Buffer.create 256 in
  (b, Format.formatter_of_buffer b)

let shown b ppf =
  Format.pp_print_flush ppf ();
  Buffer.contents b

let has affix s =
  Alcotest.(check bool) affix true (Astring.String.is_infix ~affix s)

let test_run () =
  let b, ppf = buffer () in
  let root = ref None in
  let v =
    Driver.run ~ppf ~progress:`Plain (fun r ->
        root := Some r;
        Task.run r "work" (fun t ->
            Task.log t "a line";
            42))
  in
  Alcotest.(check int) "its value" 42 v;
  Alcotest.(check bool)
    "the root succeeded" true
    (Task.state (Option.get !root) = `Succeeded);
  has "work" (shown b ppf)

let test_raise () =
  let b, ppf = buffer () in
  let root = ref None in
  Alcotest.check_raises "goes on" (Failure "boom") (fun () ->
      Driver.run ~ppf ~progress:`Plain (fun r ->
          root := Some r;
          failwith "boom"));
  Alcotest.(check bool)
    "the root failed" true
    (Task.state (Option.get !root) = `Failed);
  ignore (shown b ppf : string)

let test_task_is_current () =
  let _, ppf = buffer () in
  Driver.run ~ppf ~progress:`Quiet (fun root ->
      let inner =
        Task.run root "outer" (fun outer ->
            Driver.run ~ppf (fun given -> (outer, given)))
      in
      Alcotest.(check bool)
        "a nested run gets the current task" true
        (fst inner == snd inner))

let test_output_reaches_the_display () =
  let b, ppf = buffer () in
  Driver.run ~ppf ~progress:`Plain (fun _ ->
      Driver.print (View.text "printed");
      Driver.with_output (fun out -> Fmt.pf out "written@.");
      Driver.redraw ());
  let out = shown b ppf in
  has "printed" out;
  has "written" out

let test_logs_reach_the_display () =
  let b, ppf = buffer () in
  let before = Logs.reporter () in
  let reporter =
    Driver.run ~ppf ~progress:`Plain (fun _ ->
        Logs.app (fun m -> m "in the run");
        Domain.join
          (Domain.spawn (fun () -> Logs.app (fun m -> m "in a worker")));
        Logs.reporter ())
  in
  Alcotest.(check bool) "given back" true (Logs.reporter () == before);
  Alcotest.(check bool) "taken while it ran" true (reporter != before);
  let out = shown b ppf in
  has "in the run" out;
  has "in a worker" out

let test_a_run_after_a_run () =
  let b, ppf = buffer () in
  let once text =
    Driver.run ~ppf ~progress:`Plain (fun r -> Task.run r text ignore)
  in
  once "first";
  once "second";
  let out = shown b ppf in
  has "first" out;
  has "second" out

let suite =
  ( "console_driver",
    [
      Alcotest.test_case "run" `Quick test_run;
      Alcotest.test_case "a run that raises" `Quick test_raise;
      Alcotest.test_case "the current task" `Quick test_task_is_current;
      Alcotest.test_case "output reaches the display" `Quick
        test_output_reaches_the_display;
      Alcotest.test_case "logs reach the display" `Quick
        test_logs_reach_the_display;
      Alcotest.test_case "a run after a run" `Quick test_a_run_after_a_run;
    ] )
