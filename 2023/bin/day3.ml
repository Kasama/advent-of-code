open Advent_of_code.Util

(* let ( << ) f g x = f (g x) *)
let ( >> ) f g x = g (f x)

type number = { value : int; position : int * int; length : int }
type symbol = { value : char (* ; position : int * int *) }
type element = Number of number | Symbol of symbol | Dot

let string_of_element el =
  match el with
  | Number n ->
      "Number(" ^ string_of_int n.value ^ ", ("
      ^ string_of_int (fst n.position)
      ^ ", "
      ^ string_of_int (snd n.position)
      ^ "), l = " ^ string_of_int n.length ^ ")"
  | Symbol s -> "Symbol(" ^ Char.escaped s.value ^ ")"
  | Dot -> "."

(* let is_symbol c = match c with '0' .. '9' | '.' -> false | _ -> true *)
let is_number c = match c with '0' .. '9' -> true | _ -> false

let element_at (x, y) board =
  let v = board.(y).(x) in
  let element =
    match v with
    | '.' -> Dot
    | '0' .. '9' ->
        let rec leftmost_number board (x, y) =
          (* Printf.printf "(%d, %d) = %c\n" x y board.(y).(x); *)
          if is_number board.(y).(x) then
            if x == 0 then (x, y) else leftmost_number board (x - 1, y)
          else (x + 1, y)
        in
        let rec rightmost_number board (x, y) =
          if is_number board.(y).(x) then
            if x == Array.length board - 1 then (x, y)
            else rightmost_number board (x + 1, y)
          else (x - 1, y)
        in
        let minx, _ = leftmost_number board (x, y) in
        let maxx, _ = rightmost_number board (x, y) in
        let length = maxx - minx + 1 in
        (* print_int minx; *)
        (* print_newline (); *)
        (* print_int maxx; *)
        (* print_newline (); *)
        (* print_int length; *)
        (* print_newline (); *)
        let value =
          Array.sub board.(y) minx length
          |> Array.to_seq |> String.of_seq |> int_of_string
        in
        Number { value; position = (minx, y); length }
    | _ -> Symbol { value = v (* ; position = (x, y) *) }
  in
  element

type direction =
  | Up
  | UpRight
  | Right
  | DownRight
  | Down
  | DownLeft
  | Left
  | UpLeft

let neighbor (x, y) direction board =
  let max_value = Array.length board - 1 in
  let n_x =
    match direction with
    | UpRight | Right | DownRight -> x + 1
    | UpLeft | Left | DownLeft -> x - 1
    | Up | Down -> x
  in
  let n_y =
    match direction with
    | UpRight | Up | UpLeft -> y - 1
    | DownRight | Down | DownLeft -> y + 1
    | Right | Left -> x
  in
  if n_x < 0 || n_x > max_value || n_y < 0 || n_y > max_value then Option.none
  else Option.some (n_x, n_y)

let neighbors_of (x, y) board =
  let directions =
    [ UpRight; Up; UpLeft; Left; DownLeft; Down; DownRight; Right ]
  in
  let el = board |> element_at (x, y) in
  match el with
  | Dot ->
      List.filter_map
        (fun d ->
          neighbor (x, y) d board
          |> Option.map (fun pos -> element_at pos board))
        directions
  | Symbol _ ->
      List.filter_map
        (fun d ->
          neighbor (x, y) d board
          |> Option.map (fun pos -> element_at pos board))
        directions
  | Number { position = x, y; length; _ } ->
      let positions = List.map (fun x -> (x, y)) (x -- (x + length)) in
      let neighbors =
        List.map
          (fun pos ->
            List.filter_map
              (fun d ->
                neighbor pos d board
                |> Option.map (fun pos -> element_at pos board))
              directions)
          positions
      in
      List.flatten neighbors

let p1 raw_board =
  let elements =
    Array.mapi
      (fun x l ->
        Array.mapi
          (fun y c ->
            ((x, y), element_at (x, y) raw_board, neighbors_of (x, y) raw_board))
          l
        |> Array.to_list)
      raw_board
    |> Array.to_list |> List.flatten
  in
  let numbers_with_symbol_neighbors =
    elements
    |> List.filter (fun ((x, y), element, neighbors) ->
           match element with
           | Number _ ->
               List.fold_left
                 (fun acc neighbor ->
                   acc || match neighbor with Symbol _ -> true | _ -> false)
                 false neighbors
           | _ -> false)
  in
  let part_number_sum =
    List.fold_left
      (fun sum (_, el, _) ->
        match el with Number { value = number; _ } -> sum + number | _ -> sum)
      0 numbers_with_symbol_neighbors
  in
  part_number_sum

let () =
  let input = Advent_of_code.Read.read_all () in
  let lines = String.split_on_char '\n' input in
  let y_size = lines |> List.length in
  let x_size = lines |> List.map String.length |> List.fold_left max 0 in
  let raw_board =
    List.map (String.to_seq >> Array.of_seq) lines |> Array.of_list
  in
  let el = raw_board |> element_at (7, 2) in
  print_string @@ string_of_element el;
  print_newline ();
  print_int @@ p1 raw_board;
  print_newline ();
  print_int y_size;
  print_newline ()
