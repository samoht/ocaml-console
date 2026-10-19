open Alcobar
open Console

let test s n =
  let output = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer output in
  let ctx =
    Display.ctx ~ppf
      ~now:(fun () -> 0.)
      ~wait:ignore
      ~dimensions:(fun () -> (80, 24))
      ~live:false ()
  in
  let closed = ref None in
  Display.run ~ctx (fun d ->
      ignore (Fmt.str "%a" Display.pp d);
      check
        (Display.with_output d (fun ppf ->
             Format.pp_print_string ppf s;
             42)
        = 42);
      let t = Display.task ~key:"same" d s in
      check (t == Display.task ~key:"same" d s);
      Display.set_label t s;
      Display.set_count t ~cur:(n land 255) ~total:255;
      let b = Display.task d s in
      Display.set_bytes b ~cur:(Int64.of_int (n land 255)) ~total:255L;
      List.iter
        (fun level -> Display.log ~level b s)
        [ `Info; `Warning; `Error ];
      Display.succeed t;
      Display.fail t;
      Display.cancel t;
      Display.fail b;
      let c = Display.task d s in
      Display.cancel c;
      Display.set_result ~detail:s d (`Ok s);
      Display.set_result d (`Fail s);
      closed := Some d);
  check
    (try
       ignore (Display.task (Option.get !closed) s);
       false
     with Invalid_argument _ -> true)

let suite =
  ( "api.display",
    [
      test_case "display public operations on memory formatter" [ bytes; int ]
        test;
    ] )
