let usable_width w = if w > 10 then Some w else None

let width_of_ioctl () =
  try (Eio_unix.Pty.get_window_size Eio_unix.Fd.stdout).cols |> usable_width
  with Unix.Unix_error _ -> None

let dimension_of_env name usable =
  Option.bind (Sys.getenv_opt name) (fun value ->
      Option.bind (int_of_string_opt value) usable)

let first ~default sources =
  let rec loop = function
    | [] -> default
    | source :: rest -> (
        match source () with Some w -> w | None -> loop rest)
  in
  loop sources

(* The ioctl answers whenever stdout has a terminal behind it, $COLUMNS and
   $LINES answer when the shell exported them, and 80 by 24 is what everyone
   else falls back to. No [tput] probe: it reads the same termcap entry the
   shell already consulted and costs a shell, a fork and two blocking waits
   inside this Eio driver to say so. *)
let width () =
  first ~default:80
    [ width_of_ioctl; (fun () -> dimension_of_env "COLUMNS" usable_width) ]

let usable_height h = if h > 2 then Some h else None

let height_of_ioctl () =
  try (Eio_unix.Pty.get_window_size Eio_unix.Fd.stdout).rows |> usable_height
  with Unix.Unix_error _ -> None

let height () =
  first ~default:24
    [ height_of_ioctl; (fun () -> dimension_of_env "LINES" usable_height) ]

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
