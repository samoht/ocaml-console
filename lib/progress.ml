(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = [ `Auto | `Tty | `Plain | `Quiet | `Json ]

let to_string = function
  | `Auto -> "auto"
  | `Tty -> "tty"
  | `Plain -> "plain"
  | `Quiet -> "quiet"
  | `Json -> "json"

let all =
  List.map (fun p -> (to_string p, p)) [ `Auto; `Tty; `Plain; `Quiet; `Json ]

let of_string s =
  match List.assoc_opt s all with
  | Some p -> Ok p
  | None ->
      Fmt.error_msg "unknown progress %S, expected one of: %s" s
        (String.concat ", " (List.map fst all))

module Event = struct
  type task = int * string list
  type count_unit = [ `Items | `Bytes | `Other of string ]
  type level = [ `App | `Error | `Warning | `Info | `Debug | `Other of string ]
  type status = [ `Succeeded | `Failed | `Cancelled | `Other of string ]
  type outcome = [ `Ok | `Error | `Other of string ]

  type kind =
    [ `Start of task * int option
    | `Progress of task * int64 * int64 * count_unit
    | `Label of task * string
    | `Output of task option * string * level option
    | `End of task * status * float * string option
    | `Result of outcome * string * string option
    | `Other of string ]

  type t = { time : float; kind : kind }

  let make ~time kind = { time; kind }
  let time e = e.time
  let kind e = e.kind

  (* An open enumeration: the known values, then any other string. *)
  let open_enum known =
    let of_string s =
      match List.assoc_opt s known with Some v -> v | None -> `Other s
    in
    let to_string v =
      match v with
      | `Other s -> s
      | v -> fst (List.find (fun (_, v') -> v' = v) known)
    in
    (of_string, to_string)

  let count_unit : (string -> count_unit) * (count_unit -> string) =
    open_enum [ ("items", `Items); ("bytes", `Bytes) ]

  let level : (string -> level) * (level -> string) =
    open_enum
      [
        ("app", `App);
        ("error", `Error);
        ("warning", `Warning);
        ("info", `Info);
        ("debug", `Debug);
      ]

  let status : (string -> status) * (status -> string) =
    open_enum
      [
        ("succeeded", `Succeeded); ("failed", `Failed); ("cancelled", `Cancelled);
      ]

  let outcome : (string -> outcome) * (outcome -> string) =
    open_enum [ ("ok", `Ok); ("error", `Error) ]

  (* The object as it is written: every member, in the order of the line,
     each absent when the event has none. *)
  type members = {
    time : float;
    event : string;
    id : int option;
    task : string list option;
    parent : int option;
    count : int64 option;
    total : int64 option;
    unit : string option;
    status : string option;
    elapsed : float option;
    text : string option;
    level : string option;
    detail : string option;
  }

  let none =
    {
      time = 0.;
      event = "";
      id = None;
      task = None;
      parent = None;
      count = None;
      total = None;
      unit = None;
      status = None;
      elapsed = None;
      text = None;
      level = None;
      detail = None;
    }

  let of_task ((id, labels) : task) m =
    { m with id = Some id; task = Some labels }

  let to_members (e : t) =
    let m = { none with time = e.time } in
    match e.kind with
    | `Start (task, parent) -> of_task task { m with event = "start"; parent }
    | `Progress (task, count, total, u) ->
        of_task task
          {
            m with
            event = "progress";
            count = Some count;
            total = Some total;
            unit = Some (snd count_unit u);
          }
    | `Label (task, text) ->
        of_task task { m with event = "label"; text = Some text }
    | `Output (task, text, l) -> (
        let m =
          {
            m with
            event = "output";
            text = Some text;
            level = Option.map (snd level) l;
          }
        in
        match task with
        | Some task -> of_task task m
        | None -> { m with task = Some [] })
    | `End (task, s, elapsed, text) ->
        of_task task
          {
            m with
            event = "end";
            status = Some (snd status s);
            elapsed = Some elapsed;
            text;
          }
    | `Result (o, text, detail) ->
        {
          m with
          event = "result";
          status = Some (snd outcome o);
          text = Some text;
          detail;
        }
    | `Other event -> { m with event }

  let of_members (m : members) =
    let get name = function
      | Some v -> v
      | None ->
          Jsont.Error.msgf Jsont.Meta.none "%s event without %s" m.event name
    in
    let task () = (get "id" m.id, get "task" m.task) in
    let kind =
      match m.event with
      | "start" -> `Start (task (), m.parent)
      | "progress" ->
          `Progress
            ( task (),
              get "count" m.count,
              get "total" m.total,
              fst count_unit (get "unit" m.unit) )
      | "label" -> `Label (task (), get "text" m.text)
      | "output" ->
          let task =
            match m.id with
            | None -> None
            | Some id -> Some (id, Option.value m.task ~default:[])
          in
          `Output (task, get "text" m.text, Option.map (fst level) m.level)
      | "end" ->
          `End
            ( task (),
              fst status (get "status" m.status),
              get "elapsed" m.elapsed,
              m.text )
      | "result" ->
          `Result
            (fst outcome (get "status" m.status), get "text" m.text, m.detail)
      | event -> `Other event
    in
    { time = m.time; kind }

  (* Every number is written with 15 significant digits, the most that a
     float holds exactly: then a time rounded to the millisecond is written
     as short as it can be, 1.5 or 0, and an integer exactly below 10^15.
     An integer from 10^15 is a string of decimal digits, as jsont writes an
     integer beyond 2^53-1; both forms decode. *)
  let number_format : Jsont.number_format = "%.15g"

  let integer ~to_int64 dec =
    let number =
      Jsont.map Jsont.number ~enc:(fun v -> Int64.to_float (to_int64 v))
    and digits =
      Jsont.map Jsont.string ~enc:(fun v -> Int64.to_string (to_int64 v))
    in
    let exact v =
      let n = to_int64 v in
      Int64.compare n (-1_000_000_000_000_000L) > 0
      && Int64.compare n 1_000_000_000_000_000L < 0
    in
    Jsont.any ~kind:"integer" ~dec_number:dec ~dec_string:dec
      ~enc:(fun v -> if exact v then number else digits)
      ()

  let int = integer ~to_int64:Int64.of_int Jsont.int
  let int64 = integer ~to_int64:Fun.id Jsont.int64

  (* Seconds rounded to the millisecond in [-10^12, 10^12]: at most 15
     significant digits, so that the number written reads back as it was. *)
  let max_seconds = 1e12

  let to_seconds x =
    if not (Float.is_finite x) then 0.
    else
      let x = Float.min max_seconds (Float.max (-.max_seconds) x) in
      (Float.round (x *. 1000.) /. 1000.) +. 0.

  let seconds = Jsont.map Jsont.number ~dec:to_seconds ~enc:to_seconds

  (* A C1 control, U+0080 to U+009F, acts as an escape code on a terminal
     that reads 8-bit controls; jsont escapes C0 controls and DEL. *)
  let is_c1 u =
    let c = Uchar.to_int u in
    c >= 0x80 && c <= 0x9f

  (* [s] as valid UTF-8 without a C1 control: each byte that is not part of
     a character and each C1 control is U+FFFD. *)
  let clean s =
    let rec is_clean i =
      i >= String.length s
      ||
      let d = String.get_utf_8_uchar s i in
      Uchar.utf_decode_is_valid d
      && (not (is_c1 (Uchar.utf_decode_uchar d)))
      && is_clean (i + Uchar.utf_decode_length d)
    in
    if is_clean 0 then s
    else
      let buf = Buffer.create (String.length s + 8) in
      let rec go i =
        if i < String.length s then begin
          let d = String.get_utf_8_uchar s i in
          let u = Uchar.utf_decode_uchar d in
          Buffer.add_utf_8_uchar buf
            (if Uchar.utf_decode_is_valid d && not (is_c1 u) then u
             else Uchar.rep);
          go (i + Uchar.utf_decode_length d)
        end
      in
      go 0;
      Buffer.contents buf

  let text = Jsont.map Jsont.string ~dec:Fun.id ~enc:clean

  let jsont =
    let open Jsont.Object in
    map ~kind:"console progress event"
      (fun
        time
        event
        id
        task
        parent
        count
        total
        unit
        status
        elapsed
        text
        level
        detail
      ->
        {
          time;
          event;
          id;
          task;
          parent;
          count;
          total;
          unit;
          status;
          elapsed;
          text;
          level;
          detail;
        })
    |> mem "time" seconds ~enc:(fun (m : members) -> m.time)
    |> mem "event" text ~enc:(fun m -> m.event)
    |> opt_mem "id" int ~enc:(fun m -> m.id)
    |> opt_mem "task" (Jsont.list text) ~enc:(fun m -> m.task)
    |> opt_mem "parent" int ~enc:(fun m -> m.parent)
    |> opt_mem "count" int64 ~enc:(fun m -> m.count)
    |> opt_mem "total" int64 ~enc:(fun m -> m.total)
    |> opt_mem "unit" text ~enc:(fun m -> m.unit)
    |> opt_mem "status" text ~enc:(fun m -> m.status)
    |> opt_mem "elapsed" seconds ~enc:(fun m -> m.elapsed)
    |> opt_mem "text" text ~enc:(fun m -> m.text)
    |> opt_mem "level" text ~enc:(fun m -> m.level)
    |> opt_mem "detail" text ~enc:(fun m -> m.detail)
    |> skip_unknown |> finish
    |> Jsont.map ~kind:"console progress event" ~dec:of_members ~enc:to_members

  let to_string e =
    match Jsont_bytesrw.encode_string ~number_format jsont e with
    | Ok s -> s
    | Error e -> invalid_arg e

  let of_string s =
    match Jsont_bytesrw.decode_string jsont s with
    | Ok e -> Ok e
    | Error e -> Error (`Msg e)

  let pp ppf e = Format.pp_print_string ppf (to_string e)
end
