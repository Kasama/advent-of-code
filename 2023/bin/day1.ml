let parse_line line =
  let is_number c = match c with '0' .. '9' -> true | _ -> false in
  let chars = line |> String.to_seq |> List.of_seq in
  let first_number =
    List.find_opt is_number chars |> Option.value ~default:'0'
  in
  let last_number =
    List.find_opt is_number (List.rev chars) |> Option.value ~default:'0'
  in
  let buf = Buffer.create 2 in
  Buffer.add_char buf first_number;
  Buffer.add_char buf last_number;
  print_string (Buffer.contents buf);
  print_string " <- ";
  print_endline line;
  Buffer.contents buf |> int_of_string_opt |> Option.value ~default:0

let part_1 lines =
  let calibration_values = List.map parse_line lines in
  List.fold_left ( + ) 0 calibration_values

let part_2 (lines : string list) =
  let replacements =
    [
      (Re.compile @@ Re.str @@ "oneight", "18");
      (Re.compile @@ Re.str @@ "threeight", "38");
      (Re.compile @@ Re.str @@ "fiveight", "58");
      (Re.compile @@ Re.str @@ "sevenine", "79");
      (Re.compile @@ Re.str @@ "eightwo", "82");
      (Re.compile @@ Re.str @@ "eighthree", "83");
      (Re.compile @@ Re.str @@ "twone", "21");
      (Re.compile @@ Re.str @@ "nineight", "98");
      (Re.compile @@ Re.str @@ "one", "1");
      (Re.compile @@ Re.str @@ "two", "2");
      (Re.compile @@ Re.str @@ "three", "3");
      (Re.compile @@ Re.str @@ "four", "4");
      (Re.compile @@ Re.str @@ "five", "5");
      (Re.compile @@ Re.str @@ "six", "6");
      (Re.compile @@ Re.str @@ "seven", "7");
      (Re.compile @@ Re.str @@ "eight", "8");
      (Re.compile @@ Re.str @@ "nine", "9");
    ]
  in
  let new_lines =
    List.map
      (fun line ->
        List.fold_left
          (fun l (search, replace) ->
            Re.replace_string ~all:true search ~by:replace l)
          line replacements)
      lines
  in
  (* List.combine lines new_lines *)
  (* |> List.iter (fun (l, n) -> *)
  (*        print_int (parse_line n); *)
  (*        print_string " <- "; *)
  (*        print_string n; *)
  (*        print_string " <- "; *)
  (*        print_endline l); *)
  part_1 new_lines

let () =
  let input = Advent_of_code.Read.read_all () in
  let lines = String.split_on_char '\n' input in
  let p1 = part_1 lines in
  let _ = part_2 lines in
  print_string "Part 1: ";
  print_endline (string_of_int p1)
(* print_string "Part 2: "; *)
(* print_endline (string_of_int p2) *)
