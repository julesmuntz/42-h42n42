open%client Js_of_ocaml
open%client Ft_variables

type%client boundary = {
  mutable x : float;
  mutable y : float;
  mutable width : float;
  mutable height : float;
}

type%client quadtree = {
  mutable boundary : boundary;
  capacity : int;
  mutable objects : creet array;
  mutable count : int;
  mutable subdivided : bool;
  mutable northeast : quadtree option;
  mutable northwest : quadtree option;
  mutable southeast : quadtree option;
  mutable southwest : quadtree option;
}

let%client createBoundary x y w h : boundary = {x; y; width = w; height = h}

let%client placeholderCreet : creet =
  let div = Dom_html.createDiv Dom_html.document in
  {
    x = 0.0;
    y = 0.0;
    direction = {x = 0.0; y = 0.0};
    speed = 0.0;
    size = {width = 0.0; height = 0.0};
    status = Healthy;
    movement = (fun _ -> ());
    target = None;
    startTime = 0.0;
    creetElem = div;
    lastContact = None;
    contagionRisk = 2.0;
  }

let%client createQuadtree (boundary : boundary) (capacity : int) : quadtree =
  {
    boundary;
    capacity;
    objects = Array.make capacity placeholderCreet;
    count = 0;
    subdivided = false;
    northeast = None;
    northwest = None;
    southeast = None;
    southwest = None;
  }

let%client contains (qt : quadtree) (point_x : float) (point_y : float) : bool =
  let b = qt.boundary in
  point_x >= b.x
  && point_x < b.x +. b.width
  && point_y >= b.y
  && point_y < b.y +. b.height

let%client subdivide (qt : quadtree) : unit =
  let b = qt.boundary in
  let x = b.x in
  let y = b.y in
  let w = b.width /. 2.0 in
  let h = b.height /. 2.0 in
  let ne = createBoundary (x +. w) y w h in
  let nw = createBoundary x y w h in
  let se = createBoundary (x +. w) (y +. h) w h in
  let sw = createBoundary x (y +. h) w h in
  qt.northeast <- Some (createQuadtree ne qt.capacity) ;
  qt.northwest <- Some (createQuadtree nw qt.capacity) ;
  qt.southeast <- Some (createQuadtree se qt.capacity) ;
  qt.southwest <- Some (createQuadtree sw qt.capacity) ;
  qt.subdivided <- true

let%client rec insert (qt : quadtree) (creet : creet) : bool =
  if not (contains qt creet.x creet.y) then
    false
  else if qt.count < qt.capacity then (
    qt.objects.(qt.count) <- creet ;
    qt.count <- qt.count + 1 ;
    true
  ) else (
    if not qt.subdivided then subdivide qt ;
    let success = ref false in
    if Option.is_some qt.northeast then
      if insert (Option.get qt.northeast) creet then success := true ;
    if (not !success) && Option.is_some qt.northwest then
      if insert (Option.get qt.northwest) creet then success := true ;
    if (not !success) && Option.is_some qt.southeast then
      if insert (Option.get qt.southeast) creet then success := true ;
    if (not !success) && Option.is_some qt.southwest then
      if insert (Option.get qt.southwest) creet then success := true ;
    !success
  )

let%client clear (qt : quadtree) : unit =
  qt.count <- 0 ;
  qt.subdivided <- false ;
  qt.northeast <- None ;
  qt.northwest <- None ;
  qt.southeast <- None ;
  qt.southwest <- None

let%client rec queryRange (qt : quadtree) (range : boundary)
    (found_array : creet array) (found_index : int) : int =
  let intersects (a : boundary) (b : boundary) : bool =
    not
      ( a.x > b.x +. b.width
      || a.x +. a.width < b.x
      || a.y > b.y +. b.height
      || a.y +. a.height < b.y )
  in
  if not (intersects qt.boundary range) then
    found_index
  else
    let new_index = ref found_index in
    for i = 0 to qt.count - 1 do
      if !new_index < Array.length found_array then (
        found_array.(!new_index) <- qt.objects.(i) ;
        new_index := !new_index + 1
      )
    done ;
    if qt.subdivided then (
      if Option.is_some qt.northeast then
        new_index :=
          queryRange (Option.get qt.northeast) range found_array !new_index ;
      if Option.is_some qt.northwest then
        new_index :=
          queryRange (Option.get qt.northwest) range found_array !new_index ;
      if Option.is_some qt.southeast then
        new_index :=
          queryRange (Option.get qt.southeast) range found_array !new_index ;
      if Option.is_some qt.southwest then
        new_index :=
          queryRange (Option.get qt.southwest) range found_array !new_index
    ) ;
    !new_index

let%client getNearbyCreets (qt : quadtree) (creet : creet) : creet list =
  let search_range =
    {
      x = creet.x -. creet.size.width;
      y = creet.y -. creet.size.height;
      width = creet.size.width *. 2.0;
      height = creet.size.height *. 2.0;
    }
  in
  let nearby = Array.make 10 placeholderCreet in
  let found_count = queryRange qt search_range nearby 0 in
  Array.sub nearby 0 found_count |> Array.to_list
