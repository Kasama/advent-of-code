let read_all () =
  let buf = Buffer.create 4096 in
  let rec loop () =
    match read_line () with
    | line -> line |> Buffer.add_string buf;
      Buffer.add_char buf '\n';
      loop ()
    | exception End_of_file -> Buffer.contents buf
  in
  loop ()
