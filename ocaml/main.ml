open Raylib

let width = 700
let height = 700
let size = 100 
let w = size
let h = size
let qqty = 100
let max_hist_init = w*h*2
let xi = 1. (* evaporation rate *)
let smell_dist = 30 (* distance from top left of the border of the solution *)
let captor_radius = 10 (* radius of the captor *)
type ant = {
    pos : int * int;
    hist : (int * int) list;
    color : float;
}

type arguments =
  {
    grid : float array array;
    ants : ant list;
    max_hist : int;
  }

let shuffle d = (* shuffle a list, used in choix_pondere *)
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond

(* place an ant in the middle of the grid with a random color *)
let generate_random _ = {pos = (w/2 + Random.int 5 - 10 ,h/2 + Random.int 5 - 10); hist = []; color = Random.float 360.0}

(* simple and utils functions *)
let direcions = [ (1,0); (-1,0); (0,1); (0,-1) ]
let (+$) (x1,y1) (x2,y2) = (x1+x2,y1+y2)
let ( *$) (x,y) k = (k*x,k*y)
let dist (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2) (* manhattan distance *)
let valid (x,y) =  0<= x && x < w && 0<= y && y < h
let get grid (x,y) = if valid (x,y) then grid.(x).(y) else 0.0

(* choose an element in a list with a probability proportional to the first element of the tuple *)
let choix_pondere l =
  let total = List.fold_left (fun acc (p,_) -> acc +. p) 0.0 l in
  let choix = Random.float total in
  let rec aux s = function
    | [] -> failwith "choix_pondere" 
    | (p,x)::q -> if choix <= s+.p then x else aux (s +. p) q
  in aux 0.0 (shuffle l)

(* es ce que la position est prise en vue des autres et de son historique*)
let prise pos hist other = List.mem pos hist || Hashtbl.mem other pos

let choose grid hist other (x,y) =
  let neighbors = List.map (fun dir -> (x,y) +$ dir,dir) direcions
    |> List.filter (
        fun (pos,_) ->valid pos && not (prise pos hist other) (* not occupied *)
      )
  in
  let sum_in_dir (x,y) dir = 
      List.fold_left (+.) 0.0 (List.init captor_radius (fun k -> get grid ((x,y) +$ (dir *$ k))))
  in
  choix_pondere  (List.map (fun (pos,dir) -> sum_in_dir (x,y) dir , pos ) neighbors)

let update {grid;ants;max_hist} =
  let nmax = ref max_hist in
  let occupied = Hashtbl.create 100 in
  let ants' = List.map (fun {pos;hist;color} ->
      if dist pos (0,0) < smell_dist then
      begin (* on a trouvé la solution *)
        List.iter (fun (x,y) -> grid.(x).(y) <- 1.0) (pos::hist);
        nmax := min (List.length hist) !nmax; 
        generate_random ()
      end
      else if List.length hist > max_hist then generate_random ()
      else
        try
            let pos' = choose grid hist occupied pos in
            Hashtbl.add occupied pos' ();
        {pos = pos'; hist = pos::hist; color = color}
        with _ -> generate_random ()
        ) ants
    in
  {
    grid = Array.mapi (fun i a -> Array.mapi (fun j x -> if dist (i,j) (0,0) >= smell_dist then x *. xi else x) a) grid;
    ants = ants';
    max_hist = !nmax
  }

let draw {grid;ants;max_hist} =
  begin_drawing ();
  draw_text (Printf.sprintf "best : %d" max_hist) 10 10 20 Color.black;
  for i = 0 to w - 1 do
    for j = 0 to h - 1 do
      let x = i * (width / w) in
      let y = j * (height / h) in
      let v = int_of_float (grid.(i).(j) *. 255.) in
      let c = Color.create v v v 10 in
      draw_rectangle x y (width / w) (height / h) c
    done
  done;
  List.iter (fun {pos;color} ->
      let x, y = pos in
      let x = x * (width / w) in
      let y = y * (height / h) in
      let larg = (width / w) / 2 in
      let haut = (height / h) / 2 in
      draw_rectangle (x+larg/2) (y+haut/2) larg haut (color_from_hsv color 1.0 1.0)
    ) ants;
  end_drawing ()

let rec loop args =
  if window_should_close () then close_window () else
    begin
      draw args;
      let args' = update args in

      (if is_mouse_button_down MouseButton.Left then
        let mouse_x,mouse_y = get_mouse_x (), get_mouse_y () in
        args'.grid.(mouse_x / (width / w)).(mouse_y  / (height / h)) <- 1.0);
      loop args'
    end



let () =
  init_window width height "Hello, World!";
  Random.self_init ();
  set_target_fps 60;
  loop {
    grid = Array.init w (fun x -> Array.init h (fun y -> if dist (0,0) (x,y) < smell_dist then 1.0 else 0.0));
    ants = List.init qqty generate_random;
    max_hist = max_hist_init
  };

