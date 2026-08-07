type t = TransitGatewayRoute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TransitGatewayRoute.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayRoute.to_query v
let to_json v = `List (List.map TransitGatewayRoute.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayRoute.of_json j
