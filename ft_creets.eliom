open%client Js_of_ocaml
open%client Js_of_ocaml_lwt
open%client Ft_variables
open%client Ft_utils
open%client Ft_rendering
open%client Ft_movement
open%client Ft_collisions
open%client Ft_contagion

let%client newCreet (startTime : float) (params : parameters) =
  let size : creetSize =
    {width = creet.size.width; height = creet.size.height}
  in
  let xMargin = int_of_float (creet.size.width *. 0.7) in
  let x =
    float
      ( Random.int (map.width - int_of_float size.width - (2 * xMargin))
      + xMargin )
  in
  let spawnRange = map.spawnArea.max - map.spawnArea.min in
  let adjustedY = map.spawnArea.min + (spawnRange / 6) in
  let y =
    float
      ( Random.int (map.spawnArea.max - int_of_float size.height - adjustedY)
      + adjustedY )
  in
  let direction = newDirection () in
  let speed = params.creetSpeedAtStart in
  let status = Healthy in
  let target = None in
  let movement = moveForward in
  let creetElem = Dom_html.createDiv Dom_html.document in
  creetElem##.className := Js.string "creet" ;
  creetElem##.style##.position := Js.string "absolute" ;
  creetElem##.style##.left
  := Js.string (string_of_int (int_of_float x) ^ "px") ;
  creetElem##.style##.top
  := Js.string (string_of_int (int_of_float y) ^ "px") ;
  creetElem##.innerHTML := Js.string (getSvg status size) ;
  {
    x;
    y;
    direction;
    speed;
    size;
    status;
    target;
    movement;
    startTime;
    lastContact = None;
    contagionRisk = 2.0;
    creetElem;
  }

let%client grabCreet (creetObj : creet) (ev : Dom_html.mouseEvent Js.t)
    (board : Dom_html.divElement Js.t) (isHealed : bool ref) =
  setPos creetObj ev board ;
  creetObj.creetElem##.style##.left
  := Js.string (string_of_int (int_of_float creetObj.x) ^ "px") ;
  creetObj.creetElem##.style##.top
  := Js.string (string_of_int (int_of_float creetObj.y) ^ "px") ;
  if creetObj.y > float map.hospitalArea && creetObj.status <> Healthy then (
    creetObj.status <- Healthy ;
    updateSvg creetObj ;
    isHealed := true
  ) ;
  Lwt.return ()

let%client releaseCreet (creetObj : creet) (isHealed : bool ref)
    (endLoop : bool ref) =
  if !endLoop then
    Lwt.return ()
  else (
    if creetObj.y > float map.hospitalArea && creetObj.status <> Healthy then (
      creetObj.status <- Healthy ;
      updateSvg creetObj ;
      isHealed := true
    ) ;
    Lwt.return ()
  )

let%client generateCreet (board : Dom_html.divElement Js.t)
    (creetList : creet list ref) (healthyCreets : creet list ref)
    (globalEnd : bool ref) (startTime : float) (params : parameters) =
  let creetObj = newCreet startTime params in
  let endLoop = ref false in
  let isSelected = ref false in
  let isHealed = ref false in
  Dom.appendChild board creetObj.creetElem ;
  creetList := creetObj :: !creetList ;
  healthyCreets := creetObj :: !healthyCreets ;
  let rec updateLoop () =
    let%lwt () = Lwt_js.sleep 0.0005 in
    if not !isSelected then (
      changeDirection creetObj board ;
      areaCollision creetObj ;
      creetObj.movement creetObj ;
      checkContagion creetObj board endLoop isSelected isHealed creetList
        healthyCreets params
    ) ;
    if !globalEnd then endLoop := true ;
    if !endLoop then (
      creetList := List.filter (fun c -> c != creetObj) !creetList ;
      healthyCreets := List.filter (fun c -> c != creetObj) !healthyCreets ;
      Dom.removeChild board creetObj.creetElem ;
      Lwt.return ()
    ) else
      updateLoop ()
  in
  Lwt.async (fun () -> updateLoop ()) ;
  let rec checkEndLoop () =
    let%lwt () = Lwt_js.sleep 0.1 in
    if !endLoop then
      Lwt.return ()
    else
      checkEndLoop ()
  in
  Lwt.async (fun () ->
      let open Lwt_js_events in
      mousedowns creetObj.creetElem (fun ev _ ->
          isSelected := true ;
          let%lwt () = grabCreet creetObj ev board isHealed in
          Lwt.pick
            [
              mousemoves Dom_html.document (fun x _ ->
                  grabCreet creetObj x board isHealed );
              checkEndLoop ();
              (let%lwt _ = mouseup Dom_html.document in
               isSelected := false ;
               releaseCreet creetObj isHealed endLoop );
            ] ) )

let%client updateLoop (creetList : creet list ref)
    (board : Dom_html.divElement Js.t) (endLoop : bool ref)
    (isSelected : bool ref) (isHealed : bool ref)
    (healthyCreets : creet list ref) (params : parameters) =
  let continue = ref true in
  let%lwt () =
    while%lwt !continue do
      let%lwt () = Lwt_js.sleep 0.01 in
      List.iter
        (fun creet ->
          creet.movement creet ;
          checkContagion creet board endLoop isSelected isHealed creetList
            healthyCreets params )
        !creetList ;
      if !endLoop then (
        continue := false ;
        Lwt.return ()
      ) else
        Lwt.return ()
    done
  in
  Lwt.return ()

let%client checkEndLoop (creetList : creet list ref) (endLoop : bool ref) =
  let continue = ref true in
  let%lwt () =
    while%lwt !continue do
      let%lwt () = Lwt_js.sleep 0.1 in
      if List.length !creetList <= 1 then (
        endLoop := true ;
        continue := false ;
        Lwt.return ()
      ) else if !endLoop then (
        continue := false ;
        Lwt.return ()
      ) else
        Lwt.return ()
    done
  in
  Lwt.return ()
