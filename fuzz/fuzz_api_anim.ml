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
    = [ s; s ]);
  let animations = [ a; Anim.map (( +. ) 1.) a; Anim.const 42. ] in
  let actual = Anim.frame (Anim.all animations) ~elapsed:t in
  let expected = List.map (fun a -> Anim.frame a ~elapsed:t) animations in
  check
    (List.for_all2
       (fun a b -> a = b || (Float.is_nan a && Float.is_nan b))
       actual expected)

let suite =
  ("api.anim", [ test_case "animation sampling laws" [ bytes; float ] test ])
