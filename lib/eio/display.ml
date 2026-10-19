(* The refresh waits on the display's own clock, the one its rows read, so a
   context whose clock a caller moves by hand paces the refresh as well. *)
let animate ~sw display =
  let wait = Console.Display.wait display in
  let running = ref true in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec loop () =
        wait 0.12;
        if !running then begin
          Console.Display.tick display;
          loop ()
        end
        else `Stop_daemon
      in
      loop ());
  fun () -> running := false

let event_level : Logs.level -> Console.Display.Event.level = function
  | Logs.Warning -> `Warning
  | Logs.Error -> `Error
  | Logs.App | Logs.Info | Logs.Debug -> `Info

type request = {
  level : Console.Display.Event.level;
  message : string;
  reply : (unit, exn) result Eio.Promise.t Eio.Promise.u;
}

let closed_reporter () =
  invalid_arg "Console_eio: the display reporter is closed"

let reporter ~owner ~closed ~requests display =
  let emit level message =
    if Option.is_some (Eio.Promise.peek closed) then closed_reporter ();
    if Int.equal (Domain.self () :> int) (owner : Domain.id :> int) then
      Console.Display.event ~level display message
    else
      Eio.Fiber.first
        (fun () ->
          let answer, reply = Eio.Promise.create () in
          Eio.Stream.add requests { level; message; reply };
          Eio.Promise.await answer |> Eio.Promise.await_exn)
        (fun () ->
          Eio.Promise.await closed;
          closed_reporter ())
  in
  let report _src level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Fmt.kstr
          (fun message ->
            if message <> "" then emit (event_level level) message;
            over ();
            k ())
          fmt)
  in
  { Logs.report }

let receive ~sw ~closed requests display =
  let rec loop () =
    let { level; message; reply } = Eio.Stream.take requests in
    let output =
      Eio.Fiber.fork_promise ~sw (fun () ->
          Eio.Fiber.first
            (fun () ->
              if Option.is_some (Eio.Promise.peek closed) then
                closed_reporter ();
              Console.Display.event ~level display message)
            (fun () ->
              Eio.Promise.await closed;
              closed_reporter ()))
    in
    Eio.Promise.resolve reply output;
    ignore (Eio.Promise.await output : (unit, exn) result);
    Eio.Fiber.check ();
    loop ()
  in
  loop ()

let run ~ctx ?mode ?theme ?palette ?layout ?width ?height ?header f =
  Console.Display.run ~ctx ?mode ?theme ?palette ?layout ?width ?height ?header
    (fun display ->
      Eio.Switch.run @@ fun sw ->
      let owner = Domain.self () in
      let requests = Eio.Stream.create 1 in
      let closed, close = Eio.Promise.create () in
      let previous = Logs.reporter () in
      Eio.Fiber.fork_daemon ~sw (fun () -> receive ~sw ~closed requests display);
      Logs.set_reporter (reporter ~owner ~closed ~requests display);
      Fun.protect
        (fun () ->
          let stop = animate ~sw display in
          Fun.protect (fun () -> f display) ~finally:stop)
        ~finally:(fun () ->
          Logs.set_reporter previous;
          Eio.Promise.resolve close ()))
