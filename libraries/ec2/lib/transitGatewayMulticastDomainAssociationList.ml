type t = TransitGatewayMulticastDomainAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayMulticastDomainAssociation.parse (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list TransitGatewayMulticastDomainAssociation.to_query v

let to_json v = `List (List.map TransitGatewayMulticastDomainAssociation.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayMulticastDomainAssociation.of_json j
