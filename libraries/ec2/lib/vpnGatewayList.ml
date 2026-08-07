type t = VpnGateway.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VpnGateway.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpnGateway.to_query v
let to_json v = `List (List.map VpnGateway.to_json v)
let of_json j = Aws.Json.to_list VpnGateway.of_json j
