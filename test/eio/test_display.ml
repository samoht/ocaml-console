(* Console_eio.Display driver. Everything runs in [`Plain] mode onto a buffer, where
   each task line, log line and the result banner is emitted as plain text, so
   the output is deterministic and the escape-code redraw path stays out. *)

module D = Console_eio.Display

(* Run [f] under a fresh display captured into a string, then return the
   text. *)
let capture ?header f =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Eio.Switch.run (fun sw ->
      D.run ~sw ~clock ~ppf ~mode:`Plain ?header (fun t -> f env sw clock t));
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let is_infix ~affix s =
  let n = String.length affix and m = String.length s in
  let rec at i = i + n <= m && (String.sub s i n = affix || at (i + 1)) in
  n = 0 || at 0

let mem ~msg needle haystack =
  Alcotest.(check bool) msg true (is_infix ~affix:needle haystack)

let absent ~msg needle haystack =
  Alcotest.(check bool) msg false (is_infix ~affix:needle haystack)

(* A finished task commits its label; set_result `Ok prints the [OK] banner. *)
let test_run_banner () =
  let out =
    capture ~header:"Build" (fun _env _sw _clock t ->
        let step = D.task t "compile foo.ml" in
        D.finish step;
        D.set_result t ~detail:"3 files, 0 errors" (`Ok "build succeeded"))
  in
  mem ~msg:"task label" "compile foo.ml" out;
  mem ~msg:"detail" "3 files, 0 errors" out;
  mem ~msg:"verdict marker" "[OK]" out;
  mem ~msg:"verdict message" "build succeeded" out

(* set_result `Fail prints the [FAIL] banner. *)
let test_run_fail () =
  let out =
    capture (fun _env _sw _clock t ->
        let step = D.task t "link" in
        D.finish ~error:true step;
        D.set_result t (`Fail "link failed"))
  in
  mem ~msg:"fail marker" "[FAIL]" out;
  mem ~msg:"fail message" "link failed" out

(* An unhandled exception clears the region and propagates. *)
let test_run_raises () =
  Alcotest.check_raises "propagates" (Failure "boom") (fun () ->
      ignore
        (capture (fun _env _sw _clock t ->
             let _ = D.task t "doomed" in
             failwith "boom")))

(* A Logs message tagged with a task lands in that task's window; an untagged
   one falls back to the top-level [logs] task. *)
let test_reporter_tag () =
  let out =
    capture (fun _env _sw _clock t ->
        Logs.set_reporter (D.reporter t);
        Logs.set_level (Some Logs.App);
        let a = D.task t "step-a" in
        let b = D.task t "step-b" in
        Logs.app (fun m -> m ~tags:(D.tag a) "for-a");
        Logs.app (fun m -> m ~tags:(D.tag b) "for-b");
        Logs.app (fun m -> m "untagged-line");
        D.finish a;
        D.finish b)
  in
  mem ~msg:"tagged a" "for-a" out;
  mem ~msg:"tagged b" "for-b" out;
  mem ~msg:"untagged routed to fallback" "untagged-line" out;
  mem ~msg:"fallback task created" "logs" out

(* pipe drains a source line by line into the task window. *)
let test_pipe () =
  let out =
    capture (fun _env sw clock t ->
        let step = D.task t "fetch" in
        let src = Eio.Flow.string_source "alpha\nbeta\ngamma\n" in
        D.pipe ~sw step src;
        Eio.Time.sleep clock 0.05;
        D.finish step)
  in
  mem ~msg:"first line" "alpha" out;
  mem ~msg:"middle line" "beta" out;
  mem ~msg:"last line" "gamma" out

(* follow tails a file into the task window; a half-written trailing line is
   held back until its newline arrives on a later append. *)
let test_follow () =
  let out =
    capture (fun env sw clock t ->
        let path =
          Eio.Path.(
            Eio.Stdenv.fs env / Filename.temp_file "tty-eio-follow" ".log")
        in
        Eio.Path.save ~create:(`Or_truncate 0o644) path "one\ntwo\npartial";
        let step = D.task t "deploy" in
        D.follow ~sw ~clock ~poll:0.02 step path;
        Eio.Time.sleep clock 0.06;
        Eio.Path.save ~append:true ~create:(`If_missing 0o644) path
          "-rest\nthree\n";
        Eio.Time.sleep clock 0.06;
        Eio.Path.unlink path;
        D.finish step)
  in
  mem ~msg:"first line" "one" out;
  mem ~msg:"second line" "two" out;
  mem ~msg:"completed partial line" "partial-rest" out;
  mem ~msg:"line after append" "three" out;
  absent ~msg:"no half-written partial" "partial\n" out

let suite =
  ( "display",
    [
      ("run banner", `Quick, test_run_banner);
      ("run fail", `Quick, test_run_fail);
      ("run raises", `Quick, test_run_raises);
      ("reporter tag", `Quick, test_reporter_tag);
      ("pipe", `Quick, test_pipe);
      ("follow", `Quick, test_follow);
    ] )
