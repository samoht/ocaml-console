(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let positive n = if n > 0 then Some n else None

(* A display draws at most 4096 columns or rows, so a larger [$COLUMNS] or
   [$LINES] is taken as unset. *)
let usable n = if n > 0 && n <= 4096 then Some n else None

external window_size : Unix.file_descr -> int = "console_tty_window_size"
[@@noalloc]

let window_size fd =
  match window_size fd with
  | -1 -> None
  | size -> Some (size lsr 16, size land 0xffff)

let window_size_of = function None -> None | Some fd -> window_size fd

let width_of_ioctl fd () =
  Option.bind (window_size_of fd) (fun (w, _) -> positive w)

let dimension_of_env name usable =
  Option.bind (Sys.getenv_opt name) (fun value ->
      Option.bind (Recording.decimal value) usable)

let first ~default sources =
  Option.value ~default (List.find_map (fun source -> source ()) sources)

(* POSIX XBD 8.3: when set, COLUMNS and LINES override the size of the terminal.
   There is no [tput] fallback: it would fork only to read the termcap entry
   that the shell already read. *)
let width fd =
  first ~default:80
    [ (fun () -> dimension_of_env "COLUMNS" usable); width_of_ioctl fd ]

let height_of_ioctl fd () =
  Option.bind (window_size_of fd) (fun (_, h) -> positive h)

let height fd =
  first ~default:24
    [ (fun () -> dimension_of_env "LINES" usable); height_of_ioctl fd ]

let asked () =
  Option.map (fun w () -> (w, height None)) (dimension_of_env "COLUMNS" usable)

let dimensions fd = (width fd, height fd)

(* The descriptor that [ppf] writes to, when [ppf] is {!Fmt.stdout} or
   {!Fmt.stderr}. *)
let descriptor ppf =
  if ppf == Fmt.stdout then Some Unix.stdout
  else if ppf == Fmt.stderr then Some Unix.stderr
  else None

let is_terminal fd = Unix.isatty fd && not (Colour.dumb ~getenv:Sys.getenv_opt)

let of_ppf ppf =
  match descriptor ppf with
  | Some unix when is_terminal unix -> Some unix
  | _ -> None

(* Two descriptors on one terminal have the same device. *)
let device unix =
  match Unix.fstat unix with
  | st -> Some st.Unix.st_rdev
  | exception Unix.Unix_error _ -> None

(* The standard formatters on the same terminal as [unix], whose lines the
   display prints above its region. A standard formatter on another terminal
   writes straight out. *)
let routed unix =
  let on_display std =
    match of_ppf std with
    | Some u -> Option.equal Int.equal (device u) (device unix)
    | None -> false
  in
  List.filter on_display [ Fmt.stdout; Fmt.stderr ]

(* macOS keeps hundreds of unused ttyp* to ttyw*, so its own ttys* come
   before the other tty*, such as OpenBSD's pseudo-terminals. *)
let device_names ~dev ~pts =
  let tty n = n <> "tty" && String.starts_with ~prefix:"tty" n in
  let macos, others =
    List.partition (String.starts_with ~prefix:"ttys") (List.filter tty dev)
  in
  let in_dir dir = List.map (Filename.concat dir) in
  ("/dev/tty" :: in_dir "/dev/pts" pts) @ in_dir "/dev" (macos @ others)

(* [reopened unix] is the terminal of [unix] opened again, found by its
   device, so that the last resort is written on it without blocking and
   without changing the flags of a descriptor that the program shares with
   its shell. *)
let reopened unix =
  let rdev name =
    match Unix.stat name with
    | st when st.Unix.st_kind = Unix.S_CHR -> Some st.st_rdev
    | _ | (exception Unix.Unix_error _) -> None
  in
  let opened d name =
    match
      Unix.openfile name
        [ Unix.O_WRONLY; Unix.O_NONBLOCK; Unix.O_NOCTTY; Unix.O_CLOEXEC ]
        0
    with
    | exception Unix.Unix_error _ -> None
    | tty when (Unix.fstat tty).st_rdev = d -> Some tty
    | tty ->
        Unix.close tty;
        None
  in
  let entries dir =
    match Sys.readdir dir with
    | names -> Array.to_list names
    | exception Sys_error _ -> []
  in
  match device unix with
  | None -> None
  | Some d ->
      (* [stat] on /dev/tty gives the device of /dev/tty itself: only an
         opened descriptor tells the device of the controlling terminal. *)
      List.find_map
        (fun name ->
          if name = "/dev/tty" || rdev name = Some d then opened d name
          else None)
        (device_names ~dev:(entries "/dev") ~pts:(entries "/dev/pts"))

let file unix =
  match Unix.fstat unix with
  | st -> Some (st.Unix.st_dev, st.Unix.st_ino)
  | exception Unix.Unix_error _ -> None

(* The standard formatters that write where [unix] does. *)
let sharing unix =
  let shares std =
    match (descriptor std, file unix) with
    | Some u, Some f -> file u = Some f
    | _ -> false
  in
  List.filter shares [ Fmt.stdout; Fmt.stderr ]

let cursor unix visible =
  let s = if visible then "\027[?25h" else "\027[?25l" in
  try ignore (Unix.write_substring unix s 0 (String.length s))
  with Unix.Unix_error _ -> ()

let size ppf = Option.map (fun unix () -> dimensions (Some unix)) (of_ppf ppf)

(* The widest a display draws, whatever the terminal says. *)
let max_width = 4096

let width_of ppf =
  let first (size : unit -> int * int) = fst (size ()) in
  if Option.is_some (Cast.file ()) then Some (first Recording.size)
  else
    match size ppf with
    | Some size -> Some (Int.min max_width (first size))
    | None -> Option.map first (asked ())
