open%server Eliom_content.Html.D
open%server Eliom_content
open%client Js_of_ocaml
open%client Js_of_ocaml_lwt
open%client Ft_creets
open%client Ft_utils
open%client Ft_variables

module%server H42n42_app = Eliom_registration.App (struct
  let application_name = "h42n42"

  let global_data_path = None
end)

let%server mapElem =
  div
    ~a:[a_class ["map"]]
    [
      div ~a:[a_class ["river-area"]] [txt "RIVER"];
      div ~a:[a_class ["hospital-area"]] [txt "HOSPITAL"];
    ]

let%server populationElem = div ~a:[a_class ["population-counter"]] []

let%server gameOverElem = div ~a:[a_class ["game-over"]] [txt "GAME OVER"]

let%server parametersForm =
  div
    ~a:[a_class ["parameters"]]
    [
      div
        [
          txt "Initial Speed: ";
          input
            ~a:
              [
                a_input_type `Text;
                a_class ["param-input"];
                a_value "0.10";
                a_id "speedInput";
              ]
            ();
        ];
      div
        [
          txt "Initial Count: ";
          input
            ~a:
              [
                a_input_type `Text;
                a_class ["param-input"];
                a_value "1";
                a_id "countInput";
              ]
            ();
        ];
      div
        [
          txt "Time to Die: ";
          input
            ~a:
              [
                a_input_type `Text;
                a_class ["param-input"];
                a_value "20";
                a_id "timeInput";
              ]
            ();
        ];
      div
        [
          txt "Spawn Speed: ";
          input
            ~a:
              [
                a_input_type `Text;
                a_class ["param-input"];
                a_value "2";
                a_id "spawnSpeedInput";
              ]
            ();
        ];
    ]

let%client getInputValue id =
  Js.Opt.case
    (Dom_html.document##getElementById (Js.string id))
    (fun () -> None)
    (fun element ->
      Js.Opt.case
        (Dom_html.CoerceTo.input element)
        (fun () -> None)
        (fun input -> Some (float_of_string (Js.to_string input##.value))) )

let%client start () =
  if not !isGameActive then (
    let mapElement = Eliom_content.Html.To_dom.of_div ~%mapElem in
    let population = Eliom_content.Html.To_dom.of_div ~%populationElem in
    let gameOver = Eliom_content.Html.To_dom.of_div ~%gameOverElem in
    let creetList = ref [] in
    let healthyCreets = ref [] in
    let globalEnd = ref false in
    let startTime = (new%js Js.date_now)##getTime in
    let creetsContainer = Dom_html.createDiv Dom_html.document in
    gameOver##.style##.display := Js.string "none" ;
    creetsContainer##.className := Js.string "creets-container" ;
    Dom.appendChild mapElement creetsContainer |> ignore ;
    let parameters =
      {
        creetSpeedAtStart =
          ( match getInputValue "speedInput" with
          | Some v -> v
          | None -> 0.10 );
        totalCreetsStart =
          ( match getInputValue "countInput" with
          | Some v -> v
          | None -> 1.0 );
        timeToDie =
          ( match getInputValue "timeInput" with
          | Some v -> v
          | None -> 10.0 );
        creetSpawnSpeed =
          ( match getInputValue "spawnSpeedInput" with
          | Some v -> v
          | None -> 5.0 );
      }
    in
    for _ = 0 to int_of_float parameters.totalCreetsStart - 1 do
      generateCreet mapElement creetList healthyCreets globalEnd startTime parameters
    done ;
    let loop () =
      let p =
        let rec gameLoop () =
          let%lwt () = Lwt_js.sleep parameters.creetSpawnSpeed in
          generateCreet mapElement creetList healthyCreets globalEnd startTime
            parameters ;
          gameLoop ()
        in
        gameLoop ()
      in
      let rec checkEndgame () =
        let%lwt () = Lwt_js.sleep 0.1 in
        healthyCreets := List.filter (fun c -> c.status = Healthy) !creetList ;
        let healthy_count = List.length !healthyCreets in
        let total_count = List.length !creetList in
        if healthy_count > 0 then (
          population##.className := Js.string "population-counter has-content" ;
          population##.innerHTML :=
            Js.string
              ( "<span style='color: #94b0c4; font-size: 20px;'>Not contaminated: "
              ^ string_of_int healthy_count
              ^ "/"
              ^ string_of_int total_count
              ^ "</span>" ) ;
          checkEndgame ()
        ) else (
          population##.className := Js.string "population-counter" ;
          population##.innerHTML := Js.string "" ;
          Lwt.cancel p ;
          globalEnd := true ;
          isGameActive := false ;
          gameOver##.style##.display := Js.string "flex" ;
          Lwt.return ()
        )
      in
      checkEndgame ()
    in
    isGameActive := true ;
    Lwt.async (fun () -> loop ())
  )

let%client init () =
  disableEvent Dom_html.Event.dragstart Dom_html.document ;
  disableEvent Dom_html.Event.drop Dom_html.document ;
  let mapElement = Eliom_content.Html.To_dom.of_div ~%mapElem in
  mapElement##.style##.width := Js.string (string_of_int map.width ^ "px") ;
  mapElement##.style##.height := Js.string (string_of_int map.height ^ "px") ;
  Random.self_init ()

let%server page () =
  Eliom_tools.F.html ~title:"h42n42"
    ~css:[["css"; "styles.css"]]
    Html.F.(
      body
        [
          div [gameOverElem; mapElem];
          div
            [
              parametersForm;
              button
                ~a:
                  [
                    a_class ["start-button"];
                    a_onclick [%client fun _ -> start ()];
                  ]
                [txt "START"];
            ];
          populationElem;
        ] )

let%server main =
  Eliom_service.create ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%server () =
  H42n42_app.register ~service:main (fun () () ->
      let _ = [%client (init () : unit)] in
      Lwt.return (page ()) )

let%server () =
  Ocsigen_server.start
    ~ports:[(`All, 8080)]
    ~veryverbose:() ~debugmode:true ~logdir:"local/var/log/h42n42"
    ~datadir:"local/var/data/h42n42" ~uploaddir:(Some "/tmp")
    ~usedefaulthostname:true ~command_pipe:"local/var/run/h42n42-cmd"
    ~default_charset:(Some "utf-8")
    [
      Ocsigen_server.host
        [Staticmod.run ~dir:"local/var/www/h42n42" (); Eliom.run ()];
    ]
