open Angstrom
open Advent_of_code.Parse

type color = Red | Blue | Green

let string_of_color c =
  match c with Red -> "red" | Blue -> "blue" | Green -> "green"

let red = string "red" >>| fun _ -> Red
let blue = string "blue" >>| fun _ -> Blue
let green = string "green" >>| fun _ -> Green
let color = red <|> blue <|> green

type cube = { amount : int; color : color }

let string_of_cube cube =
  string_of_int cube.amount ^ " " ^ string_of_color cube.color

let (cube : cube t) =
  map2 (integer <* whitespace) (color <* whitespace) ~f:(fun n c ->
      { amount = n; color = c })

type set = cube list

let string_of_set (set : set) =
  String.concat ", " @@ List.map string_of_cube set

let set = sep_by (whitespace *> comma <* whitespace) cube

type game = { number : int; sets : set list }

let _string_of_game game =
  "Game " ^ string_of_int game.number ^ ": " ^ String.concat "; "
  @@ List.map string_of_set game.sets

let game =
  map2
    (string "Game " *> integer <* colon <* whitespace)
    (sep_by1 (whitespace *> semicolon <* whitespace) set)
    ~f:(fun n s -> { number = n; sets = s })

type cube_counter = { red : int; green : int; blue : int }

let cube_counter_map_color color f cube_counter =
  match color with
  | Red -> { cube_counter with red = f cube_counter.red }
  | Green -> { cube_counter with green = f cube_counter.green }
  | Blue -> { cube_counter with blue = f cube_counter.blue }

let is_counter_possible expected actual =
  expected.red >= actual.red
  && expected.blue >= actual.blue
  && expected.green >= actual.green

let min_cube_content game =
  let cube_counter = { red = 0; green = 0; blue = 0 } in
  let update_cube cube_counter cube =
    cube_counter |> cube_counter_map_color cube.color (max cube.amount)
  in
  List.fold_left (List.fold_left update_cube) cube_counter game.sets

let cube_counter_power cube_counter =
  cube_counter.red * cube_counter.green * cube_counter.blue

let games = sep_by end_of_line game

let p1 games =
  let claimed_content = { red = 12; green = 13; blue = 14 } in
  let min_content = List.map min_cube_content games in
  let games_with_min_content = List.combine games min_content in
  let valid_games =
    List.filter_map
      (fun (game, content) ->
        if is_counter_possible claimed_content content then Some game.number
        else None)
      games_with_min_content
  in
  List.fold_left ( + ) 0 valid_games

let p2 games =
  let min_content = List.map min_cube_content games in
  let powers = List.map cube_counter_power min_content in
  List.fold_left ( + ) 0 powers

let () =
  let input = Advent_of_code.Read.read_all () in
  let parsed = parse_string ~consume:All games input in
  let _ =
    match parsed with
    | Ok games ->
        print_string "Part 1: ";
        print_int @@ p1 games;
        print_newline ();
        print_string "Part 2: ";
        print_int @@ p2 games
    | Error e ->
        print_string "Error parsing input: ";
        print_string e
  in
  print_newline ()
