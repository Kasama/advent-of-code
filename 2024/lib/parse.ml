open Angstrom

let is_whitespace c = match c with ' ' | '\t' -> true | _ -> false
let whitespace = take_while is_whitespace
let semicolon = char ';'
let colon = char ':'
let comma = char ','
let is_digit c = match c with '0' .. '9' -> true | _ -> false
let integer = take_while1 is_digit >>| int_of_string
