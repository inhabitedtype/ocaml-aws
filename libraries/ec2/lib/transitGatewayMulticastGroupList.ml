type t = TransitGatewayMulticastGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayMulticastGroup.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayMulticastGroup.to_query v
let to_json v = `List (List.map TransitGatewayMulticastGroup.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayMulticastGroup.of_json j
