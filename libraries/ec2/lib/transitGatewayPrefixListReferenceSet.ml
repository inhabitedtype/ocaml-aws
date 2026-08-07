type t = TransitGatewayPrefixListReference.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TransitGatewayPrefixListReference.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TransitGatewayPrefixListReference.to_query v
let to_json v = `List (List.map TransitGatewayPrefixListReference.to_json v)
let of_json j = Aws.Json.to_list TransitGatewayPrefixListReference.of_json j
