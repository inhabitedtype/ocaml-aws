type t = PlacementGroupStrategy.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PlacementGroupStrategy.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PlacementGroupStrategy.to_query v
let to_json v = `List (List.map PlacementGroupStrategy.to_json v)
let of_json j = Aws.Json.to_list PlacementGroupStrategy.of_json j
