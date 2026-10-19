module P = Console_eio.Prompt

let prompt ?interactive actions =
  let flow = Eio_mock.Flow.make "prompt" in
  let out = Buffer.create 64 in
  Eio_mock.Flow.on_read flow actions;
  (P.v ?interactive ~ppf:(Format.formatter_of_buffer out) flow, out)

let got expected = function
  | Ok s -> Alcotest.(check string) "answer" expected s
  | Error (`Msg m) -> Alcotest.fail m

let refused = function
  | Error (`Msg _) -> ()
  | Ok _ -> Alcotest.fail "expected refusal"

let byte_reads s =
  List.init (String.length s) (fun i -> `Return (String.make 1 s.[i]))

let test_bytewise () =
  Eio_mock.Backend.run @@ fun () ->
  let t, out =
    prompt ~interactive:true
      (byte_reads " alpha \r\nbeta\n" @ [ `Raise End_of_file ])
  in
  Alcotest.(check bool) "interactive override" true (P.interactive t);
  got " alpha " (P.line t "first: ");
  got "beta" (P.line t "second: ");
  refused (P.line t "last: ");
  Alcotest.(check string)
    "questions flushed" "first: second: last: " (Buffer.contents out)

let test_utf8_split () =
  Eio_mock.Backend.run @@ fun () ->
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int 0x754c);
  let s = Buffer.contents b in
  let t, _ =
    prompt ~interactive:true
      [
        `Return (String.sub s 0 1);
        `Return (String.sub s 1 (String.length s - 1) ^ "\nnext\n");
        `Raise End_of_file;
      ]
  in
  got s (P.line t "utf8: ");
  got "next" (P.line t "next: ")

exception Read_failure

let test_read_failure () =
  Eio_mock.Backend.run @@ fun () ->
  let t, out =
    prompt ~interactive:true [ `Return "part"; `Raise Read_failure ]
  in
  Alcotest.check_raises "read exception propagates" Read_failure (fun () ->
      ignore (P.line t "question: "));
  Alcotest.(check string)
    "flushed before fault" "question: " (Buffer.contents out)

let test_questions () =
  Eio_mock.Backend.run @@ fun () ->
  let t, out =
    prompt ~interactive:true
      (byte_reads " YES \nno\nsecret-word\nwrong\n" @ [ `Raise End_of_file ])
  in
  Alcotest.(check (result bool string))
    "yes" (Ok true)
    (Result.map_error (fun (`Msg s) -> s) (P.confirm t "go?"));
  Alcotest.(check (result bool string))
    "no" (Ok false)
    (Result.map_error (fun (`Msg s) -> s) (P.confirm t "go?"));
  (match P.expect t "type: " "secret-word" with
  | Ok () -> ()
  | Error (`Msg s) -> Alcotest.fail s);
  (match P.expect t "type: " "secret-word" with
  | Ok () -> Alcotest.fail "mismatch accepted"
  | Error (`Msg s) ->
      Alcotest.(check bool)
        "no word in error" false
        (Astring.String.is_infix ~affix:"secret-word" s));
  Alcotest.(check string)
    "question syntax" "go? [y/N] go? [y/N] type: type: " (Buffer.contents out);
  refused (P.confirm t "end?");
  refused (P.expect t "end?" "secret-word")

let test_nonterminal () =
  Eio_mock.Backend.run @@ fun () ->
  List.iter
    (fun interactive ->
      let t, out = prompt ?interactive [ `Raise Read_failure ] in
      Alcotest.(check bool) "mock has no terminal" false (P.interactive t);
      refused (P.line t "line");
      refused (P.confirm t "confirm");
      refused (P.expect t "expect" "word");
      refused (P.secret t "secret");
      Alcotest.(check string) "no output" "" (Buffer.contents out))
    [ None; Some false ];
  let t, out = prompt ~interactive:true [ `Raise Read_failure ] in
  refused (P.secret t "secret");
  Alcotest.(check string) "no masking without tty" "" (Buffer.contents out)

let test_limit () =
  Eio_mock.Backend.run @@ fun () ->
  let t, _ = prompt ~interactive:true (byte_reads (String.make 4097 'a')) in
  refused (P.line t "too long: ")

let test_run_clock () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  let out = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer out in
  let value =
    Console_eio.run
      ~clock:(clock :> float Eio.Time.clock_ty Eio.Resource.t)
      ~ppf ~header:"build"
      (fun d ->
        let task = Console.Display.task d "compile" in
        Eio.Fiber.both
          (fun () ->
            Eio.Time.sleep clock 0.5;
            Console.Display.succeed task)
          (fun () ->
            Eio.Fiber.yield ();
            Eio_mock.Clock.set_time clock 0.5);
        Console_eio.run
          ~clock:(clock :> float Eio.Time.clock_ty Eio.Resource.t)
          ~ppf
          (fun nested ->
            Alcotest.(check bool) "same display" true (nested == d));
        42)
  in
  Alcotest.(check int) "result" 42 value;
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "task output" true
    (Astring.String.is_infix ~affix:"compile" (Buffer.contents out));
  Alcotest.(check bool)
    "no cursor codes" false
    (String.contains (Buffer.contents out) '\027')

exception Body_failure

let test_run_failure () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  let out = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer out in
  Alcotest.check_raises "body exception" Body_failure (fun () ->
      Console_eio.run
        ~clock:(clock :> float Eio.Time.clock_ty Eio.Resource.t)
        ~ppf
        (fun d ->
          ignore (Console.Display.task d "abandoned");
          raise Body_failure));
  Console_eio.run
    ~clock:(clock :> float Eio.Time.clock_ty Eio.Resource.t)
    ~ppf
    (fun _ -> ());
  Format.pp_print_flush ppf ();
  Alcotest.(check bool)
    "cancelled task" true
    (Astring.String.is_infix ~affix:"-- interrupted" (Buffer.contents out))

let test_nested_fiber () =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.make () in
  let ppf = Format.formatter_of_buffer (Buffer.create 64) in
  Console_eio.run
    ~clock:(clock :> float Eio.Time.clock_ty Eio.Resource.t)
    ~ppf
    (fun d ->
      Eio.Fiber.both
        (fun () ->
          Console_eio.run
            ~clock:(clock :> float Eio.Time.clock_ty Eio.Resource.t)
            ~ppf
            (fun nested ->
              Alcotest.(check bool) "inherited display" true (d == nested)))
        Eio.Fiber.yield)

let test_setup () =
  Eio_mock.Backend.run @@ fun () ->
  let a = Fmt.style_renderer Fmt.stdout and b = Fmt.style_renderer Fmt.stderr in
  Fun.protect
    ~finally:(fun () ->
      Fmt.set_style_renderer Fmt.stdout a;
      Fmt.set_style_renderer Fmt.stderr b)
    (fun () ->
      List.iter
        (fun renderer ->
          Console_eio.setup ~style_renderer:renderer ();
          Alcotest.(check bool)
            "stdout" true
            (Fmt.style_renderer Fmt.stdout = renderer);
          Alcotest.(check bool)
            "stderr" true
            (Fmt.style_renderer Fmt.stderr = renderer))
        [ `None; `Ansi_tty ];
      Console_eio.setup ())

let suite =
  ( "mock",
    [
      Alcotest.test_case "bytewise CRLF and buffer reuse" `Quick test_bytewise;
      Alcotest.test_case "split UTF-8 and buffered second line" `Quick
        test_utf8_split;
      Alcotest.test_case "read fault propagates after flush" `Quick
        test_read_failure;
      Alcotest.test_case "confirm and expect scripted answers" `Quick
        test_questions;
      Alcotest.test_case "nonterminal prompts never read" `Quick
        test_nonterminal;
      Alcotest.test_case "answer limit" `Quick test_limit;
      Alcotest.test_case "run with mock clock" `Quick test_run_clock;
      Alcotest.test_case "run failure releases display" `Quick test_run_failure;
      Alcotest.test_case "nested child fiber shares display" `Quick
        test_nested_fiber;
      Alcotest.test_case "setup under mock backend" `Quick test_setup;
    ] )
