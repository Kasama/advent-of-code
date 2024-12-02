open Angstrom
open Advent_of_code.Parse

let line = both (integer <* whitespace) (integer <* whitespace)
let lines = sep_by end_of_line line

let linesIntoLists (lines : (int * int) list) =
  let leftList, rightList =
    List.fold_left
      (fun lists elements ->
        let listL, listR = lists in
        let elL, elR = elements in
        (listL @ [ elL ], listR @ [ elR ]))
      ([], []) lines
  in
  (leftList, rightList)

let part_1 (lines : (int * int) list) =
  let left, right = linesIntoLists lines in
  let intSort l = List.sort ( - ) l in
  let sortedLeft = intSort left |> List.to_seq in
  let sortedRight = intSort right |> List.to_seq in

  let distance =
    Seq.zip sortedLeft sortedRight
    |> Seq.map (fun (a, b) -> a - b |> abs) (* get distance of each pair *)
    |> Seq.fold_left ( + ) 0 (* add distances together *)
  in

  distance

module Int_tbl = Hashtbl.Make (struct
  type t = int

  let equal = ( == )
  let hash = Hashtbl.hash
end)

let part_2 lines =
  let left, right = linesIntoLists lines in
  let (lookupTable : int Int_tbl.t) =
    Int_tbl.create (right |> List.length)
  in

  List.iter
    (fun element ->
      if Int_tbl.mem lookupTable element |> not then
        Int_tbl.add lookupTable element
          (right |> List.filter (( == ) element) |> List.length))
    left;

  List.fold_left
    (fun acc element ->
      let similarity_score =
        Int_tbl.find_opt lookupTable element |> Option.value ~default:0
      in
      acc + (element * similarity_score))
    0 left

let () =
  let input = Advent_of_code.Read.read_all () in
  let parsed = parse_string ~consume:All lines input in
  let _ =
    match parsed with
    | Ok lines ->
        let p1 = part_1 lines in
        let p2 = part_2 lines in
        print_string "Part 1: ";
        print_endline (string_of_int p1);
        print_string "Part 2: ";
        print_endline (string_of_int p2)
    | Error e ->
        print_string "Error parsing input: ";
        print_string e
  in
  print_newline ()
