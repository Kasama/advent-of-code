open Angstrom
open Advent_of_code.Parse
module IntSet = Set.Make (Int)

type card = {
  number : int;
  winning_numbers : IntSet.t;
  numbers_we_have : int list;
}

let numbers = sep_by1 whitespace integer

let string_of_numbers numbers =
  List.map string_of_int numbers |> String.concat " "

let card_number = string "Card" *> whitespace *> integer <* string ":"
let string_of_card_number number = "Card " ^ string_of_int number ^ ": "

let card =
  map4
    (card_number <* whitespace)
    numbers
    (whitespace *> string "|" <* whitespace)
    (numbers <* whitespace)
    ~f:(fun n w _ h ->
      { number = n; winning_numbers = w |> IntSet.of_list; numbers_we_have = h })

let string_of_card card =
  string_of_card_number card.number
  ^ string_of_numbers (card.winning_numbers |> IntSet.to_list)
  ^ " | "
  ^ string_of_numbers card.numbers_we_have

let cards = sep_by1 end_of_line card
let string_of_cards cards = List.map string_of_card cards |> String.concat "\n"

let correct_numbers card =
  List.filter
    (fun n -> IntSet.exists (( == ) n) card.winning_numbers)
    card.numbers_we_have

let p1 cards =
  let correct_numbers = List.map correct_numbers cards in
  let points =
    List.map
      (fun matches -> Int.shift_left 1 (List.length matches - 1))
      correct_numbers
  in
  List.fold_left ( + ) 0 points

let p2 cards =
  let card_mapper = Array.init (List.length cards) (fun _ -> 1) in
  let total_cards =
    List.fold_left
      (fun mapper card ->
        let correct_numbers = correct_numbers card |> List.length in
        let card_index = card.number - 1 in
        let copies = Array.get mapper card_index in
        let _ =
          Array.mapi_inplace
            (fun i n ->
              (* 2 after cardidx 2 .. 4*)
              if i <= card_index || i > card_index + correct_numbers then n
              else n + copies)
            mapper
        in
        mapper)
      card_mapper cards
  in
  Array.fold_left ( + ) 0 total_cards

let () =
  let input = Advent_of_code.Read.read_all () in
  let parsed = parse_string ~consume:All cards input in
  match parsed with
  | Ok cards ->
      (* print_string @@ string_of_cards cards; *)
      (* print_newline (); *)
      print_string "Part 1: ";
      print_int @@ p1 cards;
      print_newline ();
      print_string "Part 2: ";
      print_int @@ p2 cards;
      print_newline ()
  | Error e ->
      print_string "something went wrong: ";
      print_string e;
      print_newline ();
      ()
