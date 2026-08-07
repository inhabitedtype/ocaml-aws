type t = TransitGatewayRouteTableAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayRouteTableAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayRouteTableAssociation.to_query v
let to_json v = `List (List.map TransitGatewayRouteTableAssociation.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayRouteTableAssociation.of_json j
