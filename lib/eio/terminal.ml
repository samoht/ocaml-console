external winsize : Unix.file_descr -> int = "caml_console_winsize"
external winsize_rows : Unix.file_descr -> int = "caml_console_winsize_rows"

let usable_width w = if w > 10 then Some w else None

let width_of_ioctl () =
  try winsize Unix.stdout |> usable_width
  with Unix.Unix_error _ | Failure _ -> None

let width_of_env () =
  Option.bind (Sys.getenv_opt "COLUMNS") (fun cols ->
      Option.bind (int_of_string_opt cols) usable_width)

let width_of_tput () =
  try
    let ic = Unix.open_process_in "tput cols 2>/dev/null" in
    let result =
      try input_line ic |> int_of_string_opt with End_of_file -> None
    in
    ignore (Unix.close_process_in ic);
    Option.bind result usable_width
  with Unix.Unix_error _ -> None

let first_width sources =
  let rec loop = function
    | [] -> 80
    | source :: rest -> (
        match source () with Some w -> w | None -> loop rest)
  in
  loop sources

let width () =
  (* Try ioctl TIOCGWINSZ first (most reliable), then $COLUMNS, then tput. *)
  first_width [ width_of_ioctl; width_of_env; width_of_tput ]

let usable_height h = if h > 2 then Some h else None

let height_of_ioctl () =
  try winsize_rows Unix.stdout |> usable_height
  with Unix.Unix_error _ | Failure _ -> None

let height_of_env () =
  Option.bind (Sys.getenv_opt "LINES") (fun rows ->
      Option.bind (int_of_string_opt rows) usable_height)

let height_of_tput () =
  try
    let ic = Unix.open_process_in "tput lines 2>/dev/null" in
    let result =
      try input_line ic |> int_of_string_opt with End_of_file -> None
    in
    ignore (Unix.close_process_in ic);
    Option.bind result usable_height
  with Unix.Unix_error _ -> None

let height () =
  let rec loop = function
    | [] -> 24
    | source :: rest -> (
        match source () with Some h -> h | None -> loop rest)
  in
  loop [ height_of_ioctl; height_of_env; height_of_tput ]

let is_tty () = Unix.isatty Unix.stdout
let dimensions () = (width (), height ())
let show_cursor ppf = Fmt.pf ppf "\027[?25h%!"
let hide_cursor ppf = Fmt.pf ppf "\027[?25l%!"

(* Run [f] with a SIGINT handler that calls [cleanup] before the process dies,
   so a CTRL-C during a cursor-hidden region still restores the cursor. The
   handler reinstalls the default disposition and re-raises SIGINT, so the
   program still terminates -- with a SIGINT wait status -- as the user expects.
   The previously installed handler is restored when [f] returns or raises;
   nesting is safe, since each call saves and restores the handler it replaced. *)
let on_interrupt cleanup f =
  let previous =
    Sys.signal Sys.sigint
      (Sys.Signal_handle
         (fun signum ->
           cleanup ();
           Sys.set_signal Sys.sigint Sys.Signal_default;
           Unix.kill (Unix.getpid ()) signum))
  in
  Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigint previous) f

let rec write_all fd s off =
  if off < String.length s then
    match Unix.write_substring fd s off (String.length s - off) with
    | 0 -> raise End_of_file
    | n -> write_all fd s (off + n)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd s off

let write fd s = write_all fd s 0

let masked_attributes attr =
  {
    attr with
    Unix.c_icanon = false;
    c_echo = false;
    c_echoe = false;
    c_echok = false;
    c_echonl = false;
    c_vmin = 1;
    c_vtime = 0;
  }

let read_secret ?(prompt = "") () =
  if not (Unix.isatty Unix.stdin) then
    Error (`Msg "cannot prompt for a secret because stdin is not a terminal")
  else
    try
      let original = Unix.tcgetattr Unix.stdin in
      let restore () = Unix.tcsetattr Unix.stdin Unix.TCSANOW original in
      write Unix.stderr prompt;
      on_interrupt restore (fun () ->
          Fun.protect ~finally:restore (fun () ->
              Unix.tcsetattr Unix.stdin Unix.TCSANOW
                (masked_attributes original);
              let input = Console.Input.v ~mask:"*" () in
              let buf = Bytes.create 64 in
              let rec loop () =
                match Unix.read Unix.stdin buf 0 (Bytes.length buf) with
                | 0 -> Error (`Msg "terminal reached end of input")
                | n -> (
                    let echo, lines =
                      Console.Input.feed input (Bytes.sub_string buf 0 n)
                    in
                    write Unix.stderr echo;
                    match lines with line :: _ -> Ok line | [] -> loop ())
                | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
              in
              loop ()))
    with
    | Unix.Unix_error (e, fn, arg) ->
        Error (`Msg (Fmt.str "%s(%s): %s" fn arg (Unix.error_message e)))
    | End_of_file -> Error (`Msg "terminal output closed while prompting")
