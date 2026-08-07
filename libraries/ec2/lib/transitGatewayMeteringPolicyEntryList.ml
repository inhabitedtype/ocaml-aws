type t = TransitGatewayMeteringPolicyEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayMeteringPolicyEntry.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayMeteringPolicyEntry.to_query v
let to_json v = `List (List.map TransitGatewayMeteringPolicyEntry.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayMeteringPolicyEntry.of_json j
