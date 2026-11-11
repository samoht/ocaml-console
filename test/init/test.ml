(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let root = Filename.concat Filename.parent_dir_name Filename.parent_dir_name

(* A source file of console's libraries. A dependency such as cstruct also puts
   its sources under [lib/], so the file must exist in this tree. *)
let ours (file, _) =
  String.starts_with ~prefix:"lib/" file
  && Sys.file_exists (Filename.concat root file)

(* Console's initialisers allocate only the values that its interfaces name: the
   themes, the codecs of Task.Event and Cast, the borders, the progress layouts,
   the modules that [include] copies, and the keys, exceptions and atomics of
   its modules. The bound is just above what they allocate. If a release of
   OCaml or jsont makes them allocate more, this test fails, and the bound must
   be raised on purpose. *)
let bound = 6336

(* A program builds the layout of its rows when it loads. This layout has a
   constant non-ASCII piece that fits its room. *)
let layout =
  Console.Progress.Layout.(
    list
      [
        const (Console.View.fit (fun _ -> Console.View.text "\u{25b8}"));
        label ();
      ])

let words = List.filter ours (Init_first.words ())
let total charged = List.fold_left (fun n (_, w) -> n + w) 0 charged
let pp_file ppf (file, w) = Fmt.pf ppf "%6d %s" w file

let test_sampled () =
  (* The codec of Task.Event is a value of its interface, built when Event
     loads. So a total of 0 below would mean that the sampler saw nothing. *)
  Alcotest.(check bool)
    "words are charged to lib/event.ml" true
    (List.exists (fun (file, w) -> file = "lib/event.ml" && w > 0) words)

let test_no_table () =
  let charged = List.sort (fun (_, a) (_, b) -> Int.compare b a) words in
  (* Catches a table or a cache built when a module loads instead of at first
     use. *)
  if total charged > bound then
    Alcotest.failf
      "@[<v>console's initialisers allocate %d words, over %d:@,%a@]"
      (total charged) bound
      Fmt.(list ~sep:cut pp_file)
      charged

let test_layout () =
  ignore (Sys.opaque_identity layout);
  (* Catches a layout that draws its constant pieces when it is built. Drawing
     measures their cells, which builds the cache of character widths at load
     time. *)
  let measured =
    List.assoc_opt "test/init/test.ml"
      (Init_first.words ~through:"lib/width/console_width.ml" ())
  in
  Alcotest.(check (option int))
    "words a layout built at load allocates through Console_width" None measured

let () =
  Alcotest.run "init"
    [
      ( "init",
        [
          Alcotest.test_case "the sampler sees console load" `Quick test_sampled;
          Alcotest.test_case "loading console builds no table" `Quick
            test_no_table;
          Alcotest.test_case "a layout built at load measures nothing" `Quick
            test_layout;
        ] );
    ]
