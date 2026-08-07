type t = RouteServer.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RouteServer.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RouteServer.to_query v
let to_json v = `List (List.map RouteServer.to_json v)
let of_json j = Aws.Json.to_list RouteServer.of_json j
