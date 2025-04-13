open%client Js_of_ocaml
open%client Ft_variables

let%client getSvg (status : creetStatus) (size : creetSize) =
  let color =
    match status with
    | Healthy -> "lightgray"
    | Contaminated -> "#f7882e"
    | Berserk -> "#cc5646"
    | Mean -> "#346de9"
  in
  let dotSize =
    match status with
    | Mean -> int_of_float (size.width *. 0.34)
    | _ -> int_of_float (size.width *. 0.4)
  in
  let centerX = int_of_float (size.width /. 2.0) in
  let centerY = int_of_float (size.height /. 2.0) in
  let hitboxSize = int_of_float (size.width *. creet.hitbox) in
  "<svg width='"
  ^ string_of_int (int_of_float size.width)
  ^ "' height='"
  ^ string_of_int (int_of_float size.height)
  ^ "' viewBox='0 0 "
  ^ string_of_int (int_of_float size.width)
  ^ " "
  ^ string_of_int (int_of_float size.height)
  ^ "' xmlns='http://www.w3.org/2000/svg'>"
  ^ "<ellipse style='fill:"
  ^ color
  ^ "; fill-opacity:0.2; stroke:none' "
  ^ "cx='"
  ^ string_of_int centerX
  ^ "' "
  ^ "cy='"
  ^ string_of_int centerY
  ^ "' "
  ^ "rx='"
  ^ string_of_int hitboxSize
  ^ "' "
  ^ "ry='"
  ^ string_of_int hitboxSize
  ^ "'></ellipse>"
  ^ "<ellipse style='fill:"
  ^ color
  ^ "; stroke:none' "
  ^ "cx='"
  ^ string_of_int centerX
  ^ "' "
  ^ "cy='"
  ^ string_of_int centerY
  ^ "' "
  ^ "rx='"
  ^ string_of_int dotSize
  ^ "' "
  ^ "ry='"
  ^ string_of_int dotSize
  ^ "'></ellipse></svg>"

let%client updateSvg (creetObj : creet) =
  let svgString = getSvg creetObj.status creetObj.size in
  creetObj.creetElem##.innerHTML := Js.string svgString ;
  creetObj.creetElem##.style##.width
  := Js.string (string_of_int (int_of_float creetObj.size.width) ^ "px") ;
  creetObj.creetElem##.style##.height
  := Js.string (string_of_int (int_of_float creetObj.size.height) ^ "px")
