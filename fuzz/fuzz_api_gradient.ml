open Alcobar
open Console

let test n r c p =
  let length = 1 + (n land 63) in
  let r = r mod 1000 and c = c mod 1000 in
  let colors = [ Color.red; Color.blue ] in
  List.iter
    (fun spread ->
      let g = Gradient.v ~spread ~direction:(1, 1) ~length colors in
      check (Gradient.stops g = colors);
      check (Color.to_rgb (Gradient.at g 0.) = Color.to_rgb Color.red);
      check (Color.to_rgb (Gradient.at g 1.) = Color.to_rgb Color.blue);
      check
        (Gradient.stops (Gradient.map (Color.scale 0.5) g)
        = List.map (Color.scale 0.5) colors);
      check (Gradient.equal g (Gradient.map Fun.id g));
      check
        (Color.equal
           (Gradient.cell (Gradient.shift ~row:r ~column:c g) ~row:2 ~column:3)
           (Gradient.cell g ~row:(r + 2) ~column:(c + 3)));
      let k = r + c in
      let modulo k m = ((k mod m) + m) mod m in
      let q =
        match spread with
        | `Pad -> float_of_int k /. float_of_int length
        | `Repeat -> float_of_int (modulo k length) /. float_of_int length
        | `Reflect ->
            let x =
              float_of_int (modulo k (2 * length)) /. float_of_int (2 * length)
            in
            1. -. Float.abs ((2. *. x) -. 1.)
      in
      check (Color.equal (Gradient.cell g ~row:r ~column:c) (Gradient.at g q));
      ignore (Gradient.at g p);
      ignore (Fmt.str "%a" Gradient.pp g))
    [ `Pad; `Repeat; `Reflect ];
  check
    (try
       ignore (Gradient.v ~length:0 colors);
       false
     with Invalid_argument _ -> true);
  check
    (try
       ignore (Gradient.v ~length []);
       false
     with Invalid_argument _ -> true)

let suite =
  ( "api.gradient",
    [
      test_case "gradient coordinate and mapping laws" [ int; int; int; float ]
        test;
    ] )
