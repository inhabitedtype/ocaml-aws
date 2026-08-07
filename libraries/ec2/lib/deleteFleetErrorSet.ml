type t = DeleteFleetErrorItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DeleteFleetErrorItem.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list DeleteFleetErrorItem.to_query v
let to_json v = `List (List.map DeleteFleetErrorItem.to_json v)
let of_json j = Aws.Json.to_list DeleteFleetErrorItem.of_json j
