open%client Js_of_ocaml

type%client creetStatus =
  | Healthy
  | Contaminated
  | Berserk
  | Mean

type%client direction = {
  mutable x : float;
  mutable y : float;
}

type%client creetSize = {
  mutable width : float;
  mutable height : float;
}

type%client creet = {
  mutable x : float;
  mutable y : float;
  mutable direction : direction;
  mutable speed : float;
  size : creetSize;
  mutable status : creetStatus;
  mutable movement : creet -> unit;
  mutable target : creet option;
  startTime : float;
  mutable lastContact : creet option;
  mutable contagionRisk : float;
  creetElem : Dom_html.divElement Js.t;
}

type%client parameters = {
  creetSpeedAtStart : float;
  totalCreetsStart : float;
  timeToDie : float;
  creetSpawnSpeed : float;
}

type%client spawnArea = {
  min : int;
  max : int;
}

type%client mapConfig = {
  width : int;
  height : int;
  riverArea : int;
  hospitalArea : int;
  spawnArea : spawnArea;
}

type%client creetConfig = {
  size : creetSize;
  hitbox : float;
  lastContact : creet option;
  contagionRisk : float;
}

type%client movementConfig = {
  initialSpeed : float;
  contaminatedSpeedScale : float;
  accelerationRate : float;
}

type%client contagionConfig = {
  mutationRisk : float;
  transformationRate : float;
  meanSizeScale : float;
}

let%client map =
  {
    width = 720;
    height = 720;
    riverArea = int_of_float (float 720 *. 0.1);
    hospitalArea = int_of_float (float 720 *. 0.85);
    spawnArea =
      {
        min = int_of_float (float 720 *. 0.2);
        max = int_of_float (float 720 *. 0.7);
      };
  }

let%client creet =
  {
    size = {width = 45.0; height = 45.0};
    hitbox = 0.50;
    lastContact = None;
    contagionRisk = 2.0;
  }

let%client movement =
  {
    initialSpeed = 0.5;
    contaminatedSpeedScale = 0.85;
    accelerationRate = 0.00005;
  }

let%client contagion =
  {
    mutationRisk = 10.0;
    transformationRate = 0.025;
    meanSizeScale = 0.85;
  }

let%client isGameActive = ref false
