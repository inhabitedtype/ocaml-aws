type t = PortRange.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PortRange.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PortRange.to_query v
let to_json v = `List (List.map PortRange.to_json v)
let of_json j = Aws.Json.to_list PortRange.of_json j
