type t = CreateFleetError.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CreateFleetError.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CreateFleetError.to_query v
let to_json v = `List (List.map CreateFleetError.to_json v)
let of_json j = Aws.Json.to_list CreateFleetError.of_json j
