type t = TransitGatewayVpcAttachment.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayVpcAttachment.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayVpcAttachment.to_query v
let to_json v = `List (List.map TransitGatewayVpcAttachment.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayVpcAttachment.of_json j
