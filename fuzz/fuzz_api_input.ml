open Alcobar
open Console

let test s =
  let make () =
    Input.v ~prompt:s ~history:[ s ]
      ~complete:(fun word -> [ word; word ^ "x" ])
      ()
  in
  let a = make () and b = make () in
  check (Input.pending a = "" && Input.history a = [ s ]);
  let echo, lines = Input.feed a s in
  let echoes = Buffer.create 64 and completed = ref [] in
  String.iter
    (fun c ->
      let e, l = Input.feed b (String.make 1 c) in
      Buffer.add_string echoes e;
      completed := !completed @ l)
    s;
  check (echo = Buffer.contents echoes && lines = !completed);
  check (Input.pending a = Input.pending b && Input.history a = Input.history b);
  let masked = Input.v ~mask:"*" () in
  ignore (Input.feed masked s);
  ignore (Input.feed masked "\n");
  check (Input.history masked = []);
  let expected = String.concat "\r\n" (String.split_on_char '\n' s) in
  check (Input.crlf s = expected)

let suite =
  ("api.input", [ test_case "input chunking and masking laws" [ bytes ] test ])
