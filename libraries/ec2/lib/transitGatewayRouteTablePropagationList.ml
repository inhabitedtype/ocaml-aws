type t = TransitGatewayRouteTablePropagation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayRouteTablePropagation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayRouteTablePropagation.to_query v
let to_json v = `List (List.map TransitGatewayRouteTablePropagation.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayRouteTablePropagation.of_json j
