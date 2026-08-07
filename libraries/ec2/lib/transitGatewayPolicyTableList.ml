type t = TransitGatewayPolicyTable.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayPolicyTable.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayPolicyTable.to_query v
let to_json v = `List (List.map TransitGatewayPolicyTable.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayPolicyTable.of_json j
