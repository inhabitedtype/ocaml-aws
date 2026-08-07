type t = TransitGatewayMulticastDomain.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayMulticastDomain.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayMulticastDomain.to_query v
let to_json v = `List (List.map TransitGatewayMulticastDomain.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayMulticastDomain.of_json j
