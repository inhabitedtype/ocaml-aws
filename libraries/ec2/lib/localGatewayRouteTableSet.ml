type t = LocalGatewayRouteTable.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LocalGatewayRouteTable.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LocalGatewayRouteTable.to_query v
let to_json v = `List (List.map LocalGatewayRouteTable.to_json v)
let of_json j = Aws.Json.to_list LocalGatewayRouteTable.of_json j
