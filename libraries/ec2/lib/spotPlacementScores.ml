type t = SpotPlacementScore.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SpotPlacementScore.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SpotPlacementScore.to_query v
let to_json v = `List (List.map SpotPlacementScore.to_json v)
let of_json j = Aws.Json.to_list SpotPlacementScore.of_json j
