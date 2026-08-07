type t = RouteServerEndpoint.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RouteServerEndpoint.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RouteServerEndpoint.to_query v
let to_json v = `List (List.map RouteServerEndpoint.to_json v)
let of_json j = Aws.Json.to_list RouteServerEndpoint.of_json j
