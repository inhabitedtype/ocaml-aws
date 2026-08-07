type t = LocalGatewayRoute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LocalGatewayRoute.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LocalGatewayRoute.to_query v
let to_json v = `List (List.map LocalGatewayRoute.to_json v)
let of_json j = Aws.Json.to_list LocalGatewayRoute.of_json j
