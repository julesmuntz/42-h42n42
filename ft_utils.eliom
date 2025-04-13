open%client Js_of_ocaml
open%client Js_of_ocaml_lwt
open%client Ft_variables

let%client disableEvent (event : Dom_html.dragEvent Js.t Dom_html.Event.typ)
    (htmlElem : #Dom_html.eventTarget Js.t) =
  Lwt.async (fun () ->
      Lwt_js_events.seq_loop (Lwt_js_events.make_event event)
        ~use_capture:true htmlElem (fun ev _ ->
          Dom.preventDefault ev ;
          Lwt.return () ) )

let%client distance (c1 : creet) (c2 : creet) : float =
  let c1_centerX = c1.x +. (c1.size.width /. 2.0) in
  let c1_centerY = c1.y +. (c1.size.height /. 2.0) in
  let c2_centerX = c2.x +. (c2.size.width /. 2.0) in
  let c2_centerY = c2.y +. (c2.size.height /. 2.0) in
  let dx = c1_centerX -. c2_centerX in
  let dy = c1_centerY -. c2_centerY in
  sqrt ((dx *. dx) +. (dy *. dy))

let%client setPos (creetObj : creet) (ev : Dom_html.mouseEvent Js.t)
    (board : Dom_html.divElement Js.t) =
  let (x0, y0) = Dom_html.elementClientPosition board in
  creetObj.x <-
    float
      (max
         (min
            (ev##.clientX - x0 - (int_of_float creetObj.size.width / 2))
            (map.width - int_of_float creetObj.size.width) )
         0 ) ;
  creetObj.y <-
    float
      (max
         (min
            (ev##.clientY - y0 - (int_of_float creetObj.size.height / 2))
            (map.height - int_of_float creetObj.size.height) )
         0 )

let%client restoreSize (creetObj : creet) =
  creetObj.size.width <- creet.size.width ;
  creetObj.size.height <- creet.size.height ;
  creetObj.creetElem##.style##.width
  := Js.string (string_of_int (int_of_float creetObj.size.width) ^ "px") ;
  creetObj.creetElem##.style##.height
  := Js.string (string_of_int (int_of_float creetObj.size.height) ^ "px")
