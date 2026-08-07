type t = RouteServerPeer.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RouteServerPeer.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RouteServerPeer.to_query v
let to_json v = `List (List.map RouteServerPeer.to_json v)
let of_json j = Aws.Json.to_list RouteServerPeer.of_json j
