type t = TransitGatewayAttachment.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayAttachment.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayAttachment.to_query v
let to_json v = `List (List.map TransitGatewayAttachment.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayAttachment.of_json j
