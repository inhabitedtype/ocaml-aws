type t = FleetData.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map FleetData.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list FleetData.to_query v
let to_json v = `List (List.map FleetData.to_json v)
let of_json j = Aws.Json.to_list FleetData.of_json j
