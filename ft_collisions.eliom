open%client Ft_utils
open%client Ft_variables
open%client Ft_rendering
open%client Ft_quadtree

let%client isColliding (creet1 : creet) (creet2 : creet) : bool =
  let minDistance =
    (creet1.size.width *. creet.hitbox) +. (creet2.size.width *. creet.hitbox)
  in
  let centerDistance = distance creet1 creet2 in
  centerDistance < minDistance

let%client quadtree =
  ref
    (createQuadtree
       {x = 0.0; y = 0.0; width = float map.width; height = float map.height}
       4 )

let%client updateQuadtree creetList =
  clear !quadtree ;
  List.iter (fun creet -> ignore (insert !quadtree creet)) creetList

let%client checkCollisions (creetList : creet list) (creet : creet) : bool =
  updateQuadtree creetList ;
  let nearby_creets = getNearbyCreets !quadtree creet in
  List.exists
    (fun other ->
      other != creet && isColliding creet other && other.status <> Healthy )
    nearby_creets

let%client areaCollision (creetObj : creet) =
  let potentialX = creetObj.x +. (creetObj.direction.x *. creetObj.speed) in
  let potentialY = creetObj.y +. (creetObj.direction.y *. creetObj.speed) in
  if
    int_of_float potentialX <= 0
    || ( int_of_float potentialX > map.width - int_of_float creetObj.size.width
       && creetObj.direction.x > 0. )
  then (
    creetObj.direction.x <- -.creetObj.direction.x ;
    updateSvg creetObj
  ) ;
  if
    int_of_float potentialY <= 0
    || ( int_of_float potentialY > map.height - int_of_float creetObj.size.height
       && creetObj.direction.y > 0. )
  then
    creetObj.direction.y <- -.creetObj.direction.y
