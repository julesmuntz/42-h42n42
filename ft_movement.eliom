open%client Js_of_ocaml
open%client Ft_variables
open%client Ft_rendering

let%client newDirection () =
  let degree = Random.float 360.0 in
  let x = cos (degree *. 3.14159265 /. 180.) in
  let y = sin (degree *. 3.14159265 /. 180.) in
  let norm = sqrt ((x *. x) +. (y *. y)) in
  {x = x /. norm; y = y /. norm}

let%client moveForward (creet : creet) =
  creet.speed <- creet.speed +. movement.accelerationRate;
  creet.x <- creet.x +. (creet.direction.x *. creet.speed);
  creet.y <- creet.y +. (creet.direction.y *. creet.speed);
  creet.creetElem##.style##.left := Js.string (string_of_int (int_of_float creet.x) ^ "px");
  creet.creetElem##.style##.top := Js.string (string_of_int (int_of_float creet.y) ^ "px");
  updateSvg creet

let%client moveToTarget (creetObj : creet) =
  let dir =
    match creetObj.target with
    | Some target ->
        let x = target.x -. creetObj.x in
        let y = target.y -. creetObj.y in
        let norm = sqrt ((x *. x) +. (y *. y)) in
        if norm < 0.001 then
          creetObj.direction
        else
          {x = x /. norm; y = y /. norm}
    | None -> creetObj.direction
  in
  if creetObj.direction.x *. dir.x < 0.0 then (
    creetObj.direction <- dir;
    updateSvg creetObj
  ) else
    creetObj.direction <- dir;
  moveForward creetObj

let%client changeDirection (creet : creet) (board : Dom_html.divElement Js.t) =
  let newX = creet.x +. (creet.direction.x *. creet.speed) in
  let newY = creet.y +. (creet.direction.y *. creet.speed) in
  if newX <= 0. then (
    creet.x <- 0. ;
    creet.direction.x <- abs_float creet.direction.x
  ) else if newX >= float_of_int board##.clientWidth -. creet.size.width then (
    creet.x <- float_of_int board##.clientWidth -. creet.size.width ;
    creet.direction.x <- -.abs_float creet.direction.x
  ) else
    creet.x <- newX ;
  if newY <= 0. then (
    creet.y <- 0. ;
    creet.direction.y <- abs_float creet.direction.y
  ) else if newY >= float_of_int board##.clientHeight -. creet.size.height then (
    creet.y <- float_of_int board##.clientHeight -. creet.size.height ;
    creet.direction.y <- -.abs_float creet.direction.y
  ) else
    creet.y <- newY ;
  creet.creetElem##.style##.left := Js.string (string_of_int (int_of_float creet.x) ^ "px");
  creet.creetElem##.style##.top := Js.string (string_of_int (int_of_float creet.y) ^ "px");
  updateSvg creet
