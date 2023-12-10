type map_range = { dst: int; src: int; len: int}
type att_map = { name: string; ranges: map_range list }
type data = { seeds: int list; maps: att_map list}

let print_int_list lst =
  let meat=List.fold_left (fun a b -> a ^ (if a = "" then "" else "; ") ^ b) "" (List.map string_of_int lst) in
  "[" ^ meat ^ "]"

let print_att_map m =
  m.name

let print_data data =
  Printf.sprintf "seeds: %s\n%s" (print_int_list data.seeds) (List.fold_left (^) "" (List.map print_att_map data.maps))

let parse_nums s =
  String.split_on_char ' ' s
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map int_of_string

let rec parse_map lines name acc: att_map =
  match lines with
  | [] -> { name; ranges=acc }
  | l :: ls when l.[0] >= '0' && l.[0] <= '9' ->
    Scanf.sscanf l "%d %d %d" (fun dst src len ->
      parse_map ls name ({ dst; src; len} :: acc))
  | l :: ls -> parse_map ls l acc


let push_map (lines:string list) (acc:att_map list) = match lines with
| [] -> acc
| _ -> (parse_map lines "" []) :: acc

let rec parse_maps lines cur_map acc = match lines with
| l :: ls when l = "" -> parse_maps ls cur_map acc
| l :: ls when l.[(String.length l) - 1] == ':' -> parse_maps ls [l] (push_map cur_map acc)
| l :: ls -> parse_maps ls (l :: cur_map) acc
| [] -> List.rev (push_map cur_map acc)

let parse lines =
  match lines with
  | seeds :: ls -> (
    let seed_list = match (String.split_on_char ':' seeds) with
    | _ :: numstr :: [] -> parse_nums numstr
    | _ -> failwith "parse error" in
    { seeds=seed_list; maps=(parse_maps ls [] []) })
  | _ -> failwith "parse error"

let rec apply_map ranges i =
  match ranges with
  | r :: rs ->
    if r.src <= i && (r.src + r.len) >= i then
      r.dst + (i - r.src)
    else
      apply_map rs i
  | [] -> i

let rec apply_maps maps i =
  match maps with
  | m :: ms -> apply_maps ms (apply_map m.ranges i)
  | [] -> i

let rec apply_maps_all maps seeds acc =
  match seeds with
  | s :: ss -> apply_maps_all maps ss ((apply_maps maps s) :: acc)
  | [] -> acc

let part1 data =
  let locs = apply_maps_all data.maps data.seeds [] in
  List.fold_left min max_int locs

let rec apply_maps_all_seed_range maps i n cur_min =
  if i>=n then
    cur_min
  else
    apply_maps_all_seed_range maps (i+1) n (min cur_min (apply_maps maps i))

let part2 data =
  let rec apply_maps_min maps seeds cur_min =
    match seeds with
    | start :: len :: ss -> apply_maps_min maps ss (min (apply_maps_all_seed_range maps start (start+len) cur_min) cur_min)
    | [] -> cur_min
    | _ -> invalid_arg "seeds length must be even" in
  apply_maps_min data.maps data.seeds max_int

let solve input = (
  let data = Util.read_file input |> parse in (
    print_endline (string_of_int (part1 data));
    print_endline (string_of_int (part2 data));
  )
)