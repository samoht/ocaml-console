let animate ~sw ~clock display =
  let running = ref true in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        Eio.Time.sleep clock 0.12;
        if !running then begin
          Console.Display.tick display;
          loop ()
        end
        else `Stop_daemon
      in
      loop ());
  fun () -> running := false

let active = Atomic.make false

let event_level : Logs.level -> Console.Display.Event.level = function
  | Logs.Warning -> `Warning
  | Logs.Error -> `Error
  | Logs.App | Logs.Info | Logs.Debug -> `Info

let reporter display =
  let report _src level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Fmt.kstr
          (fun message ->
            if message <> "" then
              Console.Display.event ~level:(event_level level) display message;
            over ();
            k ())
          fmt)
  in
  { Logs.report }

let run ~sw ~clock ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header f =
  if not (Atomic.compare_and_set active false true) then
    invalid_arg "Console_eio.run: another display is active";
  Fun.protect
    (fun () ->
      Console.Display.run ~ctx ?mode ?theme ?palette ?bar ?width ?height ?header
        (fun display ->
          let previous = Logs.reporter () in
          Logs.set_reporter (reporter display);
          Fun.protect
            (fun () ->
              let stop = animate ~sw ~clock display in
              Fun.protect (fun () -> f display) ~finally:stop)
            ~finally:(fun () -> Logs.set_reporter previous)))
    ~finally:(fun () -> Atomic.set active false)
