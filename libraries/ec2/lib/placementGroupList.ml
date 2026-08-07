type t = PlacementGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PlacementGroup.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PlacementGroup.to_query v
let to_json v = `List (List.map PlacementGroup.to_json v)
let of_json j = Aws.Json.to_list PlacementGroup.of_json j
