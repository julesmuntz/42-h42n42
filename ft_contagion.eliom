open%client Js_of_ocaml
open%client Js_of_ocaml_lwt
open%client Ft_variables
open%client Ft_rendering
open%client Ft_utils
open%client Ft_movement
open%client Ft_collisions

let%client creetContagion (creetObj : creet) (board : Dom_html.divElement Js.t)
    (endLoop : bool ref) (isSelected : bool ref) (isHealed : bool ref)
    (creetList : creet list ref) (healthyCreets : creet list ref)
    (params : parameters) =
  creetObj.speed <- creetObj.speed *. movement.contaminatedSpeedScale ;
  isHealed := false ;
  updateSvg creetObj ;
  let add_once lst_ref item =
    if not (List.exists (fun c -> c == item) !lst_ref) then
      lst_ref := item :: !lst_ref
  in
  let lifeCountdown () =
    let p =
      let%lwt () = Lwt_js.sleep params.timeToDie in
      let waitingSelection () =
        let continue = ref true in
        let%lwt () =
          while%lwt !continue do
            let%lwt () = Lwt_js.sleep 0.1 in
            if not !isSelected then (
              if not (creetObj.status = Healthy) then (
                Dom.removeChild board creetObj.creetElem ;
                creetList := List.filter (fun c -> c != creetObj) !creetList ;
                healthyCreets :=
                  List.filter (fun c -> c != creetObj) !healthyCreets ;
                endLoop := true
              ) ;
              continue := false ;
              Lwt.return ()
            ) else
              Lwt.return ()
          done
        in
        Lwt.return ()
      in
      waitingSelection ()
    in
    let healthCheck () =
      let continue = ref true in
      let%lwt () =
        while%lwt !continue do
          let%lwt () = Lwt_js.sleep 0.1 in
          if !isHealed then (
            Lwt.cancel p ;
            add_once creetList creetObj ;
            add_once healthyCreets creetObj ;
            continue := false ;
            Lwt.return ()
          ) else
            Lwt.return ()
        done
      in
      Lwt.return ()
    in
    healthCheck ()
  in
  Lwt.async (fun () -> lifeCountdown ())

let%client berserkContagion (creetObj : creet) (endLoop : bool ref)
    (isHealed : bool ref) (isSelected : bool ref) =
  let rec berserkTransformation () =
    let%lwt () = Lwt_js.sleep contagion.transformationRate in
    if !isHealed || !endLoop then (
      restoreSize creetObj ;
      Lwt.return ()
    ) else if !isSelected then (
      let shrinkIncrement = creet.size.width *. contagion.transformationRate in
      creetObj.size.width <-
        max (creetObj.size.width -. shrinkIncrement) creet.size.width ;
      creetObj.size.height <-
        max (creetObj.size.height -. shrinkIncrement) creet.size.height ;
      updateSvg creetObj ;
      berserkTransformation ()
    ) else
      let growthIncrement = creet.size.width *. contagion.transformationRate in
      creetObj.size.width <-
        min (creetObj.size.width +. growthIncrement) (creet.size.width *. 4.0) ;
      creetObj.size.height <-
        min (creetObj.size.height +. growthIncrement) (creet.size.height *. 4.0) ;
      updateSvg creetObj ;
      berserkTransformation ()
  in
  Lwt.async (fun () -> berserkTransformation ())

let%client meanContagion (creetObj : creet) (endLoop : bool ref)
    (isHealed : bool ref) (healthyCreets : creet list ref) =
  creetObj.size.width <- creet.size.width *. contagion.meanSizeScale ;
  creetObj.size.height <- creet.size.height *. contagion.meanSizeScale ;
  creetObj.creetElem##.style##.width
  := Js.string (string_of_int (int_of_float creetObj.size.width) ^ "px") ;
  creetObj.creetElem##.style##.height
  := Js.string (string_of_int (int_of_float creetObj.size.height) ^ "px") ;
  creetObj.movement <- moveToTarget ;
  let meanTargetReset () =
    let continue = ref true in
    let%lwt () =
      while%lwt !continue do
        let%lwt () = Lwt_js.sleep 0.1 in
        if !isHealed then (
          restoreSize creetObj ;
          creetObj.target <- None ;
          creetObj.movement <- moveForward ;
          continue := false ;
          Lwt.return ()
        ) else (
          ( if
              creetObj.target = None
              ||
              match creetObj.target with
              | Some target -> target.status <> Healthy
              | None -> false
            then
              let newTarget =
                List.fold_left
                  (fun acc x ->
                    if x.status = Healthy then
                      Some x
                    else
                      acc )
                  None !healthyCreets
              in
              creetObj.target <- newTarget ) ;
          if !endLoop then (
            continue := false ;
            Lwt.return ()
          ) else
            Lwt.return ()
        )
      done
    in
    Lwt.return ()
  in
  Lwt.async (fun () -> meanTargetReset ())

let%client contaminate (creetObj : creet) (board : Dom_html.divElement Js.t)
    (endLoop : bool ref) (isSelected : bool ref) (isHealed : bool ref)
    (creetList : creet list ref) (healthyCreets : creet list ref)
    (params : parameters) =
  let rd = Random.int 100 in
  healthyCreets := List.filter (fun x -> x != creetObj) !healthyCreets ;
  if rd < int_of_float contagion.mutationRisk then (
    creetObj.speed <- creetObj.speed *. movement.contaminatedSpeedScale ;
    if Random.int 2 = 0 then (
      creetObj.status <- Berserk ;
      creetContagion creetObj board endLoop isSelected isHealed creetList
        healthyCreets params ;
      berserkContagion creetObj endLoop isHealed isSelected
    ) else (
      creetObj.status <- Mean ;
      creetContagion creetObj board endLoop isSelected isHealed creetList
        healthyCreets params ;
      meanContagion creetObj endLoop isHealed healthyCreets
    )
  ) else (
    creetObj.status <- Contaminated ;
    creetContagion creetObj board endLoop isSelected isHealed creetList
      healthyCreets params
  )

let%client checkRiverContagion (creetObj : creet) : bool =
  creetObj.y < float map.riverArea

let%client checkContagion (creetObj : creet) (board : Dom_html.divElement Js.t)
    (endLoop : bool ref) (isSelected : bool ref) (isHealed : bool ref)
    (creetList : creet list ref) (healthyCreets : creet list ref)
    (params : parameters) =
  if creetObj.status = Healthy then
    if checkRiverContagion creetObj then
      contaminate creetObj board endLoop isSelected isHealed creetList
        healthyCreets params
    else
      let contaminatedCreets =
        List.filter (fun c -> c.status <> Healthy) !creetList
      in
      let touching_creet =
        List.find_opt (fun c -> isColliding creetObj c) contaminatedCreets
      in
      match (touching_creet, creetObj.lastContact) with
      | (Some contaminated, Some last) when contaminated == last -> ()
      | (Some contaminated, _) ->
          creetObj.lastContact <- Some contaminated ;
          if contaminated.status = Mean then
            contaminate creetObj board endLoop isSelected isHealed creetList
              healthyCreets params
          else (
            contaminated.contagionRisk <- contaminated.contagionRisk +. 2.0 ;
            if Random.float 100.0 < contaminated.contagionRisk then
              contaminate creetObj board endLoop isSelected isHealed creetList
                healthyCreets params
          )
      | (None, _) -> creetObj.lastContact <- None
