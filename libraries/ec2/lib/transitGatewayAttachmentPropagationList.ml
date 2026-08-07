type t = TransitGatewayAttachmentPropagation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayAttachmentPropagation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayAttachmentPropagation.to_query v
let to_json v = `List (List.map TransitGatewayAttachmentPropagation.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayAttachmentPropagation.of_json j
