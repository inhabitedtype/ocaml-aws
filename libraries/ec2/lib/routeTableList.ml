type t = RouteTable.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RouteTable.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RouteTable.to_query v
let to_json v = `List (List.map RouteTable.to_json v)
let of_json j = Aws.Json.to_list RouteTable.of_json j
