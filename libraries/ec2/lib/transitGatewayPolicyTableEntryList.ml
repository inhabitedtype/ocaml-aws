type t = TransitGatewayPolicyTableEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayPolicyTableEntry.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayPolicyTableEntry.to_query v
let to_json v = `List (List.map TransitGatewayPolicyTableEntry.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayPolicyTableEntry.of_json j
