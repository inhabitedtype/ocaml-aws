type t = DeleteFleetSuccessItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DeleteFleetSuccessItem.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list DeleteFleetSuccessItem.to_query v
let to_json v = `List (List.map DeleteFleetSuccessItem.to_json v)
let of_json j = Aws.Json.to_list DeleteFleetSuccessItem.of_json j
