let animate ~sw ~clock d =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Time.sleep clock 0.12;
        Console.Display.tick d
      done;
      `Stop_daemon)

type t = {
  display : Console.Display.t;
  log_lines : int;
  mutable fallback : Console.Display.row option;
  mutable result : ([ `Ok of string | `Fail of string ] * string option) option;
}

let pp ppf _ = Format.pp_print_string ppf "<tty-eio-display>"

type task = Console.Display.row

(* A log message names its task with a Logs tag; the reporter reads the tag back
   to pick the window. The printer is only used if the tag is rendered by some
   other reporter, which never carries a useful task name, so it is a label. *)
let task_tag : task Logs.Tag.def =
  Logs.Tag.def "tty-task" (fun ppf _ -> Format.pp_print_string ppf "task")

let tag task = Logs.Tag.add task_tag task Logs.Tag.empty
let add_tag task set = Logs.Tag.add task_tag task set
let underlying t = t.display

let task ?parent ?log_lines t label =
  let log_lines = Option.value log_lines ~default:t.log_lines in
  match parent with
  | Some parent -> Console.Display.row ~parent ~log_lines t.display label
  | None -> Console.Display.row ~log_lines t.display label

let finish = Console.Display.finish
let log = Console.Display.log
let set_result t ?detail result = t.result <- Some (result, detail)

(* Untagged messages land in a lazily-created top-level task so a line is never
   dropped for want of a tag. *)
let fallback_task t =
  match t.fallback with
  | Some row -> row
  | None ->
      let row = Console.Display.row ~log_lines:t.log_lines t.display "logs" in
      t.fallback <- Some row;
      row

let reporter t =
  let report _src _level ~over k msgf =
    msgf (fun ?header:_ ?tags fmt ->
        let row =
          match Option.bind tags (Logs.Tag.find task_tag) with
          | Some row -> row
          | None -> fallback_task t
        in
        Fmt.kstr
          (fun s ->
            if s <> "" then Console.Display.log row s;
            over ();
            k ())
          fmt)
  in
  { Logs.report }

let pipe ~sw task src =
  Eio.Fiber.fork ~sw (fun () ->
      let r = Eio.Buf_read.of_flow src ~max_size:(1024 * 1024) in
      let rec loop () =
        match Eio.Buf_read.line r with
        | line ->
            Console.Display.log task line;
            loop ()
        | exception End_of_file -> ()
      in
      loop ())

let follow ~sw ~clock ?(poll = 0.2) task path =
  (* Hold back the trailing partial line until its newline arrives, so a
     half-written line never shows. *)
  let leftover = ref "" in
  let consume chunk =
    let s = !leftover ^ chunk in
    let n = String.length s in
    let rec emit i =
      match String.index_from_opt s i '\n' with
      | Some j ->
          Console.Display.log task (String.sub s i (j - i));
          emit (j + 1)
      | None -> leftover := String.sub s i (n - i)
    in
    emit 0
  in
  let buf = Cstruct.create 4096 in
  let drain file off =
    let rec go off =
      let k =
        try Eio.File.pread file ~file_offset:off [ buf ] with End_of_file -> 0
      in
      if k = 0 then off
      else (
        consume (Cstruct.to_string ~len:k buf);
        go (Optint.Int63.add off (Optint.Int63.of_int k)))
    in
    go off
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop off =
        (* Re-open each tick so a file created or replaced after [follow]
           started is still picked up; [pread] then reads only the new bytes. *)
        let off =
          try Eio.Path.with_open_in path (fun file -> drain file off)
          with Eio.Io _ -> off
        in
        Eio.Time.sleep clock poll;
        loop off
      in
      loop Optint.Int63.zero)

let run ~sw ~clock ?(ppf = Fmt.stdout) ?mode ?(log_lines = 8) ?theme ?header f =
  let ctx =
    Console.Display.ctx ~ppf
      ~now:(fun () -> Eio.Time.now clock)
      ~dimensions:Terminal.dimensions ~is_tty:(Terminal.is_tty ()) ()
  in
  let display = Console.Display.v ~ctx ?mode ?theme ?header () in
  animate ~sw ~clock display;
  let t = { display; log_lines; fallback = None; result = None } in
  match f t with
  | v ->
      (match t.result with
      | Some (result, detail) ->
          Console.Display.finish_all ~result ?detail display
      | None -> Console.Display.finish_all display);
      v
  | exception e ->
      (* An unhandled failure tears the live region down cleanly and re-raises;
         a handled failure should {!set_result} [`Fail] and return instead. *)
      Console.Display.clear display;
      raise e
