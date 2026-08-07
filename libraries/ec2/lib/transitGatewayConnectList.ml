type t = TransitGatewayConnect.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TransitGatewayConnect.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayConnect.to_query v
let to_json v = `List (List.map TransitGatewayConnect.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayConnect.of_json j
