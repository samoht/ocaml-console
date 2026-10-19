open Alcobar
open Console

let test s t =
  let a = Anim.v (fun ~elapsed -> elapsed) in
  let sample = Anim.frame a ~elapsed:t in
  if t <= 0. then check (sample = 0.);
  if t > 0. then check (sample = t);
  check (Anim.frame (Anim.const s) ~elapsed:t = s);
  check
    (Anim.frame (Anim.map String.length (Anim.const s)) ~elapsed:t
    = String.length s);
  check
    (Anim.frame
       (Anim.all [ Anim.const s; Anim.map Fun.id (Anim.const s) ])
       ~elapsed:t
    = [ s; s ])

let suite =
  ("api.anim", [ test_case "animation sampling laws" [ bytes; float ] test ])
