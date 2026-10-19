open Alcobar
open Console

let test n =
  let c = Color.palette (n land 255) in
  let g = Gradient.v ~length:7 [ c; Color.red ] in
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
      Style.fg_gradient g;
      Style.bg_gradient g;
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
      check (Style.equal s (Style.map_color ~fg:Fun.id ~bg:Fun.id s));
      ignore (Style.foreground s);
      ignore (Style.background s);
      ignore (Style.is_none s);
      ignore (Style.has_gradient s);
      ignore (Style.shows_on_space s);
      ignore (Fmt.str "%a" Style.pp s);
      check (not (Style.has_gradient (Style.at ~row:n ~column:n s))))
    styles;
  check (Style.is_none Style.none);
  check (Style.foreground Style.(fg Color.red + fg c) = Some c);
  check (Style.background Style.(bg Color.red + bg c) = Some c);
  check
    (Style.foreground (Style.at ~row:2 ~column:3 (Style.fg_gradient g))
    = Some (Gradient.cell g ~row:2 ~column:3));
  List.iter
    (fun s -> check (Style.shows_on_space s))
    [ Style.bg c; Style.reverse; Style.underline; Style.strikethrough ]

let suite =
  ("api.style", [ test_case "style composition and cell laws" [ int ] test ])
