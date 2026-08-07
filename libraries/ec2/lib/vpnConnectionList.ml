type t = VpnConnection.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VpnConnection.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpnConnection.to_query v
let to_json v = `List (List.map VpnConnection.to_json v)
let of_json j = Aws.Json.to_list VpnConnection.of_json j
