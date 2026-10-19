open Alcobar
open Console

let test n =
  let c = Color.palette (n land 255) in
  let styles =
    [
      Style.none;
      Style.bold;
      Style.faint;
      Style.italic;
      Style.underline;
      Style.blink;
      Style.reverse;
      Style.strikethrough;
      Style.fg c;
      Style.bg c;
    ]
  in
  let combined =
    List.fold_left
      (fun acc (bit, s) ->
        if n land (1 lsl bit) <> 0 then Style.(acc + s) else acc)
      Style.none
      (List.mapi (fun i s -> (i, s)) styles)
  in
  let styles = combined :: styles in
  List.iter
    (fun s ->
      check (Style.equal s Style.(s + none));
      ignore (Style.foreground s);
      ignore (Style.background s);
      ignore (Style.is_none s);
      ignore (Fmt.str "%a" Style.pp s))
    styles;
  check (Style.is_none Style.none);
  check (Style.foreground Style.(fg Color.red + fg c) = Some c);
  check (Style.background Style.(bg Color.red + bg c) = Some c);
  ()

let suite = ("api.style", [ test_case "style composition laws" [ int ] test ])
