(* The API is declared and specified by the tests in test/eio/test_prompt.ml;
   the implementation lands in the TDD session that follows. Every entry point
   raises so that no caller can be built on a silent placeholder. *)

type t = unit

let v ?interactive:_ ~stdin:_ ~stdout:_ () =
  failwith "TODO: implement Console_eio.Prompt.v"

let interactive _ = failwith "TODO: implement Console_eio.Prompt.interactive"
let line _ _ = failwith "TODO: implement Console_eio.Prompt.line"
let confirm _ _ = failwith "TODO: implement Console_eio.Prompt.confirm"
let expect _ _ _ = failwith "TODO: implement Console_eio.Prompt.expect"
