type t = HostOffering.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map HostOffering.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list HostOffering.to_query v
let to_json v = `List (List.map HostOffering.to_json v)
let of_json j = Aws.Json.to_list HostOffering.of_json j
