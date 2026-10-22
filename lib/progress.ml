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
  type kind =
    [ `Start | `Progress | `Label | `Output | `End | `Result | `Other of string ]

  type count_unit = [ `Items | `Bytes | `Other of string ]
  type level = [ `App | `Error | `Warning | `Info | `Debug | `Other of string ]
  type status = [ `Succeeded | `Failed | `Cancelled | `Other of string ]
  type outcome = [ `Ok | `Error | `Other of string ]

  type t = {
    time : float;
    kind : kind;
    id : int option;
    task : string list option;
    parent : int option;
    count : int64 option;
    total : int64 option;
    unit : count_unit option;
    status : status option;
    outcome : outcome option;
    elapsed : float option;
    text : string option;
    level : level option;
    detail : string option;
  }

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

  let utf_8 = Jsont.map Jsont.string ~dec:Fun.id ~enc:clean

  (* An open enumeration: the known values, then any other string. *)
  type 'a enum = {
    of_string : string -> 'a;
    to_string : 'a -> string;
    other : string -> 'a -> 'a;
  }

  let open_enum known =
    let of_string s =
      match List.assoc_opt s known with Some v -> v | None -> `Other s
    in
    let to_string v =
      match v with
      | `Other s -> s
      | v -> fst (List.find (fun (_, v') -> v' = v) known)
    in
    (* [`Other s] with [s] listed would read back as the listed value. *)
    let other what = function
      | `Other s when List.mem_assoc s known ->
          Fmt.invalid_arg "Console.Progress.Event: %s %S is listed" what s
      | `Other s -> `Other (clean s)
      | v -> v
    in
    { of_string; to_string; other }

  let kinds : kind enum =
    open_enum
      [
        ("start", `Start);
        ("progress", `Progress);
        ("label", `Label);
        ("output", `Output);
        ("end", `End);
        ("result", `Result);
      ]

  let count_units : count_unit enum =
    open_enum [ ("items", `Items); ("bytes", `Bytes) ]

  let levels : level enum =
    open_enum
      [
        ("app", `App);
        ("error", `Error);
        ("warning", `Warning);
        ("info", `Info);
        ("debug", `Debug);
      ]

  let statuses : status enum =
    open_enum
      [
        ("succeeded", `Succeeded); ("failed", `Failed); ("cancelled", `Cancelled);
      ]

  let outcomes : outcome enum = open_enum [ ("ok", `Ok); ("error", `Error) ]

  let event ?id ?task ~time kind =
    {
      time = to_seconds time;
      kind;
      id;
      task = Option.map (List.map clean) task;
      parent = None;
      count = None;
      total = None;
      unit = None;
      status = None;
      outcome = None;
      elapsed = None;
      text = None;
      level = None;
      detail = None;
    }

  let start ?parent ~time ~id task =
    { (event ~id ~task ~time `Start) with parent }

  let progress ~time ~id ~count ~total ~unit task =
    {
      (event ~id ~task ~time `Progress) with
      count = Some count;
      total = Some total;
      unit = Some (count_units.other "unit" unit);
    }

  let label ~time ~id ~text task =
    { (event ~id ~task ~time `Label) with text = Some (clean text) }

  let output ?id ?(task = []) ?level ~time text =
    {
      (event ?id ~task ~time `Output) with
      text = Some (clean text);
      level = Option.map (levels.other "level") level;
    }

  let end' ?text ~time ~id ~status ~elapsed task =
    {
      (event ~id ~task ~time `End) with
      status = Some (statuses.other "status" status);
      elapsed = Some (to_seconds elapsed);
      text = Option.map clean text;
    }

  let result ?detail ~time ~outcome text =
    {
      (event ~time `Result) with
      outcome = Some (outcomes.other "outcome" outcome);
      text = Some (clean text);
      detail = Option.map clean detail;
    }

  let other ~time name = event ~time (kinds.other "event" (`Other name))
  let time e = e.time
  let kind e = e.kind
  let id e = e.id
  let task e = Option.value e.task ~default:[]
  let parent e = e.parent
  let count e = e.count
  let total e = e.total
  let unit e = e.unit
  let status e = e.status
  let outcome e = e.outcome
  let elapsed e = e.elapsed
  let text e = e.text
  let level e = e.level
  let detail e = e.detail

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

  let to_members (e : t) =
    let status =
      match (e.status, e.outcome) with
      | Some s, _ -> Some (statuses.to_string s)
      | None, Some o -> Some (outcomes.to_string o)
      | None, None -> None
    in
    {
      time = e.time;
      event = kinds.to_string e.kind;
      id = e.id;
      task = e.task;
      parent = e.parent;
      count = e.count;
      total = e.total;
      unit = Option.map count_units.to_string e.unit;
      status;
      elapsed = e.elapsed;
      text = e.text;
      level = Option.map levels.to_string e.level;
      detail = e.detail;
    }

  let of_members (m : members) =
    let get name = function
      | Some v -> v
      | None ->
          Jsont.Error.msgf Jsont.Meta.none "%s event without %s" m.event name
    in
    let time = m.time in
    let id () = get "id" m.id and task () = get "task" m.task in
    match kinds.of_string m.event with
    | `Start -> start ?parent:m.parent ~time ~id:(id ()) (task ())
    | `Progress ->
        let count = get "count" m.count and total = get "total" m.total in
        let unit = count_units.of_string (get "unit" m.unit) in
        progress ~time ~id:(id ()) ~count ~total ~unit (task ())
    | `Label -> label ~time ~id:(id ()) ~text:(get "text" m.text) (task ())
    | `Output ->
        let level = Option.map levels.of_string m.level in
        output ?id:m.id ?task:m.task ?level ~time (get "text" m.text)
    | `End ->
        let status = statuses.of_string (get "status" m.status) in
        end' ?text:m.text ~time ~id:(id ()) ~status
          ~elapsed:(get "elapsed" m.elapsed) (task ())
    | `Result ->
        let outcome = outcomes.of_string (get "status" m.status) in
        result ?detail:m.detail ~time ~outcome (get "text" m.text)
    | `Other name -> other ~time name

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
    |> mem "event" utf_8 ~enc:(fun m -> m.event)
    |> opt_mem "id" int ~enc:(fun m -> m.id)
    |> opt_mem "task" (Jsont.list utf_8) ~enc:(fun m -> m.task)
    |> opt_mem "parent" int ~enc:(fun m -> m.parent)
    |> opt_mem "count" int64 ~enc:(fun m -> m.count)
    |> opt_mem "total" int64 ~enc:(fun m -> m.total)
    |> opt_mem "unit" utf_8 ~enc:(fun m -> m.unit)
    |> opt_mem "status" utf_8 ~enc:(fun m -> m.status)
    |> opt_mem "elapsed" seconds ~enc:(fun m -> m.elapsed)
    |> opt_mem "text" utf_8 ~enc:(fun m -> m.text)
    |> opt_mem "level" utf_8 ~enc:(fun m -> m.level)
    |> opt_mem "detail" utf_8 ~enc:(fun m -> m.detail)
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
