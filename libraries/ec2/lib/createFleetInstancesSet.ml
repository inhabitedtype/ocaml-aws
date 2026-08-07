type t = CreateFleetInstance.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CreateFleetInstance.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CreateFleetInstance.to_query v
let to_json v = `List (List.map CreateFleetInstance.to_json v)
let of_json j = Aws.Json.to_list CreateFleetInstance.of_json j
