type t = LocalGatewayRouteTableVpcAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map LocalGatewayRouteTableVpcAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LocalGatewayRouteTableVpcAssociation.to_query v
let to_json v = `List (List.map LocalGatewayRouteTableVpcAssociation.to_json v)
let of_json j = Aws.Json.to_list LocalGatewayRouteTableVpcAssociation.of_json j
