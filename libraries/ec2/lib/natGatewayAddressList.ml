type t = NatGatewayAddress.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map NatGatewayAddress.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NatGatewayAddress.to_query v
let to_json v = `List (List.map NatGatewayAddress.to_json v)
let of_json j = Aws.Json.to_list NatGatewayAddress.of_json j
