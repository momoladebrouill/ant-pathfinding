open Raylib

let width = 800
let height = 800
let size = 100 
let w = size
let h = size
let qqty = 300
let max_hist_init = (w+h) * (w+h)
let xi = 0.999 (* evaporation rate *)
let smell_dist = 20
type ant =
  {
    pos : int * int;
    hist : (int * int) list;
  }

type arguments =
  {
    grid : float array array;
    ants : ant list;
    max_hist : int;
  }

let choix_pondere l =
  let total = List.fold_left (fun acc (p,_) -> acc +. p) 0.0 l in
  let choix = Random.float total in
  let rec aux s = function
    | [] -> failwith "choix_pondere" 
    | (p,x)::q -> if choix <= s+.p then x else aux (s +. p) q
  in
  aux 0.0 l

let get grid (x,y) = 
  if grid.(x).(y) = 1.0 then Random.float 0.005 else 10000.0*.grid.(x).(y) +. Random.float 0.003

let dist (x1,y1) (x2,y2) =
  abs (x1 - x2) + abs (y1 - y2)

let is_wall (x,y) = x mod 3 <> 0 && y mod 2 = 0

let choose grid other (x,y) =
  let neighbors = 
    [(x+1,y);(x-1,y);(x,y+1);(x,y-1)]
    |> List.rev
    |> List.filter 
    (fun (x,y) -> 
      0<= x && x < w && 0<= y && y <h
     && not (List.mem (x,y) other)
     && not (is_wall (x,y))
      ) 
    in
  let candidates = List.sort (fun a b -> Stdlib.compare (get grid a) (get grid b)) neighbors in
  choix_pondere  (List.map (fun pos -> (get grid pos), pos ) candidates)

let update {grid;ants;max_hist} =
  let nmax = ref max_hist in
  let ants' = List.map (fun {pos;hist} ->
      if dist pos (0,0) < smell_dist then
      begin
        List.iter (fun (x,y) -> grid.(x).(y) <- min (grid.(x).(y) +. 10.0) 10.0 ) (pos::hist);
        nmax := min (List.length hist) !nmax;
        {pos = (w/2,h/2); hist = []}
      end
      else if List.length hist > max_hist then
        {pos = (w/2,h/2); hist = []}
      else
        try
          {pos = choose grid hist pos; hist = pos::hist}
        with _ -> {pos = (w/2,h/2); hist = []}
        ) ants in
  {
    grid = Array.mapi (fun i a -> Array.mapi (fun j x -> if dist (i,j) (0,0) >= smell_dist then x *. xi else x) a) grid;
    ants = ants';
    max_hist = !nmax
  }

let draw {grid;ants;max_hist} =
  begin_drawing ();
  draw_text (Printf.sprintf "best : %d" max_hist) 10 10 20 Color.white;
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      let x = i * (width / w) in
      let y = j * (height / h) in
      let c = fade (color_from_hsv 100. 1. (grid.(i).(j)/.10.0)) 0.1 in
      draw_rectangle x y (width / w) (height / h) c
    done
  done;
  List.iter (fun {pos} ->
      let x, y = pos in
      let x = x * (width / w) in
      let y = y * (height / h) in
      draw_rectangle x y (width / (2*w)) (height / (2*h)) Color.brown
    ) ants;
  end_drawing ()

let rec loop args =
  if window_should_close () then close_window () else
    begin
      draw args;
      let args' = update args in

      (if is_mouse_button_down MouseButton.Left then
        let mouse_x,mouse_y = get_mouse_x (), get_mouse_y () in
        args'.grid.(mouse_x / (width / w)).(mouse_y  / (height / h)) <- 10.0);
      loop args'
    end



let () =
  init_window width height "Hello, World!";
  Random.self_init ();
  set_target_fps 60;
  loop {
    grid = Array.init w (fun i -> Array.init h 
      (fun j -> if is_wall (i,j) then 0.0 else 1.0));
    ants = List.init qqty (fun _ -> {pos = (w/2,h/2); hist = []});
    max_hist = max_hist_init
  };
  Printf.printf "Hello, World!\n";

