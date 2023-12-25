open Batteries
open Angstrom
open Advent_of_code.Parse

type range_mapper = { source_start : int; dest_start : int; range : int }

let range_mapper =
  map3 (integer <* whitespace) (integer <* whitespace)
    (integer <* whitespace <* many end_of_line)
    ~f:(fun dst src r -> { source_start = src; dest_start = dst; range = r })

let is_in_range mapper n =
  n >= mapper.source_start && n < mapper.source_start + mapper.range

let map_range mapper n =
  if is_in_range mapper n then
    let mapping = mapper.dest_start - mapper.source_start in
    n + mapping
  else n

let seeds =
  string "seeds:" *> whitespace *> sep_by1 (string " ") integer
  <* many end_of_line

let something_to_something str =
  string (str ^ " map:")
  *> whitespace *> many end_of_line
  *> sep_by (many end_of_line) range_mapper

let seed_to_soil = something_to_something "seed-to-soil"
let soil_to_fertilizer = something_to_something "soil-to-fertilizer"
let fertilizer_to_water = something_to_something "fertilizer-to-water"
let water_to_light = something_to_something "water-to-light"
let light_to_temperature = something_to_something "light-to-temperature"
let temperature_to_humidity = something_to_something "temperature-to-humidity"
let humidity_to_location = something_to_something "humidity-to-location"

type almanac = {
  seeds : int list;
  seed_to_soil : range_mapper list;
  soil_to_fertilizer : range_mapper list;
  fertilizer_to_water : range_mapper list;
  water_to_light : range_mapper list;
  light_to_temperature : range_mapper list;
  temperature_to_humidity : range_mapper list;
  humidity_to_location : range_mapper list;
}

let almanac =
  let section p = many end_of_line *> p <* whitespace in
  seeds <* whitespace >>= fun seeds ->
  section seed_to_soil >>= fun seed_to_soil ->
  section soil_to_fertilizer >>= fun soil_to_fertilizer ->
  section fertilizer_to_water >>= fun fertilizer_to_water ->
  section water_to_light >>= fun water_to_light ->
  section light_to_temperature >>= fun light_to_temperature ->
  section temperature_to_humidity >>= fun temperature_to_humidity ->
  section humidity_to_location <* many end_of_line
  >>= fun humidity_to_location ->
  return
    {
      seeds;
      seed_to_soil;
      soil_to_fertilizer;
      fertilizer_to_water;
      water_to_light;
      light_to_temperature;
      temperature_to_humidity;
      humidity_to_location;
    }

let p1 almanac =
  let apply_mapper_section n mappers =
    List.filter (fun mapper -> is_in_range mapper n) mappers
    |> List.fold_left (fun n mapper -> map_range mapper n) n
  in
  let map_seed_to_location seed =
    List.fold_left apply_mapper_section seed
      [
        almanac.seed_to_soil;
        almanac.soil_to_fertilizer;
        almanac.fertilizer_to_water;
        almanac.water_to_light;
        almanac.light_to_temperature;
        almanac.temperature_to_humidity;
        almanac.humidity_to_location;
      ]
  in
  let seed_locations = List.map map_seed_to_location almanac.seeds in
  let lowest_location = List.fold_left Int.min Int.max_num seed_locations in
  lowest_location

let () =
  let input = Advent_of_code.Read.read_all () in
  let parsed = parse_string ~consume:All almanac input in
  let _ =
    match parsed with
    | Ok almanac ->
        print_string "part 1: ";
        print_int @@ p1 almanac
    | Error e -> print_string e
  in
  print_newline ()
