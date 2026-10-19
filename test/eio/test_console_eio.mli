(** Tests for {!Console_eio.run}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the Alcotest test suite for {!Test_console_eio}. *)

val frame_suite : string * unit Alcotest.test_case list
(** [frame_suite] tests {!Console_eio.frame_drawn} and
    {!Console.Display.on_frame}. *)
