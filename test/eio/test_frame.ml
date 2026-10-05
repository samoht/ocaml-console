(* Console_eio.frame_drawn: the promise a fiber awaits to know that frame n of
   a display is on the terminal. The clocks here never move: what resolves the
   promise is the paint of the frame, so no case reads or waits on time. The
   scene draws its frame number, which is how a case reads what the terminal
   holds when the promise resolves. *)

module D = Console.Display

let is_infix ~affix s =
  let n = String.length affix and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = affix || at (i + 1)) in
  n = 0 || at 0

let resolved p = Option.is_some (Eio.Promise.peek p)

(* A context on [buf] whose clock reads zero for ever; [wait] is how the
   driver of [Console_eio.run_on] paces its refresh. *)
let context ?wait buf =
  let ppf = Format.formatter_of_buffer buf in
  Fmt.set_style_renderer ppf `None;
  ( ppf,
    D.ctx ~ppf
      ~now:(fun () -> 0.)
      ?wait
      ~dimensions:(fun () -> (40, 10))
      ~is_tty:true () )

let scene ~frame ~width:_ = [ Fmt.kstr Console.Span.text "frame %d" frame ]

let on_screen ppf buf n =
  Format.pp_print_flush ppf ();
  is_infix ~affix:(Fmt.str "frame %d" n) (Buffer.contents buf)

(* Display.on_frame: the promise for frame 2 is pending through the paint of
   frame 1 and resolved by the paint of frame 2, which is then on the
   terminal. *)
let test_resolved_by_the_paint_of_its_frame () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 1024 in
  let ppf, ctx = context buf in
  D.run ~ctx ~mode:`Activity (fun d ->
      D.set_scene d scene;
      let p = Console_eio.frame_drawn d 2 in
      Alcotest.(check bool) "pending at frame 0" false (resolved p);
      D.tick d;
      Alcotest.(check bool) "pending at frame 1" false (resolved p);
      Alcotest.(check bool) "frame 2 not drawn yet" false (on_screen ppf buf 2);
      D.tick d;
      Alcotest.(check bool) "resolved at frame 2" true (resolved p);
      Alcotest.(check bool) "frame 2 on the terminal" true (on_screen ppf buf 2);
      Alcotest.(check bool)
        "the frame on the terminal resolves at once" true
        (resolved (Console_eio.frame_drawn d 2));
      Alcotest.(check bool)
        "a frame drawn before it resolves at once" true
        (resolved (Console_eio.frame_drawn d 1)))

(* A tick while the region is lent to [with_output] moves the frame and paints
   nothing; the frame is drawn by the paint that gives the region back. *)
let test_a_lent_region_draws_on_its_return () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 1024 in
  let ppf, ctx = context buf in
  D.run ~ctx ~mode:`Activity (fun d ->
      D.set_scene d scene;
      let p = Console_eio.frame_drawn d 1 in
      D.with_output d (fun _ ->
          D.tick d;
          Alcotest.(check int) "the frame moved" 1 (D.frame d);
          Alcotest.(check bool) "pending while lent" false (resolved p));
      Alcotest.(check bool) "resolved on return" true (resolved p);
      Alcotest.(check bool) "frame 1 on the terminal" true (on_screen ppf buf 1))

(* An appending display draws no frame, so there is none to wait for. *)
let test_an_appending_display_resolves_at_once () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 64 in
  let _, ctx = context buf in
  D.run ~ctx ~mode:`History (fun d ->
      Alcotest.(check bool)
        "resolved" true
        (resolved (Console_eio.frame_drawn d 5)))

(* A frame still to come when the display closes is never drawn, and its
   promise is resolved as the display closes, so no fiber waits on it for
   ever. *)
let test_closing_resolves_what_is_to_come () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 1024 in
  let _, ctx = context buf in
  let p =
    D.run ~ctx ~mode:`Activity (fun d ->
        let p = Console_eio.frame_drawn d 10 in
        Alcotest.(check bool) "pending while open" false (resolved p);
        p)
  in
  Alcotest.(check bool) "resolved once closed" true (resolved p)

(* Under the Eio driver, a fiber awaits frame 3 while the refresh turns on a
   clock that never moves: the await ends on the refresh that draws frame 3,
   and the terminal then holds it. The wait is bounded in refreshes, not in
   time: a refresh past the eighth means the await missed its frame. *)
exception Missed_the_frame

let test_await_under_the_driver () =
  Eio_main.run @@ fun _env ->
  let buf = Buffer.create 4096 in
  let refreshes = ref 0 in
  let wait _ =
    incr refreshes;
    if !refreshes > 8 then raise Missed_the_frame;
    Eio.Fiber.yield ()
  in
  let ppf, ctx = context ~wait buf in
  Console_eio.run_on ~ctx ~mode:`Activity (fun d ->
      D.set_scene d scene;
      Eio.Promise.await (Console_eio.frame_drawn d 3);
      Alcotest.(check int) "the frame drawn" 3 (D.frame d);
      Alcotest.(check bool) "frame 3 on the terminal" true (on_screen ppf buf 3))

let suite =
  ( "frame",
    [
      Alcotest.test_case "resolved by the paint of its frame" `Quick
        test_resolved_by_the_paint_of_its_frame;
      Alcotest.test_case "a lent region draws on its return" `Quick
        test_a_lent_region_draws_on_its_return;
      Alcotest.test_case "an appending display resolves at once" `Quick
        test_an_appending_display_resolves_at_once;
      Alcotest.test_case "closing resolves what is to come" `Quick
        test_closing_resolves_what_is_to_come;
      Alcotest.test_case "await under the driver" `Quick
        test_await_under_the_driver;
    ] )
