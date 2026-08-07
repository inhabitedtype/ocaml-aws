type t = TransitGatewayRouteTable.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayRouteTable.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayRouteTable.to_query v
let to_json v = `List (List.map TransitGatewayRouteTable.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayRouteTable.of_json j
