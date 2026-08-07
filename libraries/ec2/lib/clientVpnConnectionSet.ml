type t = ClientVpnConnection.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ClientVpnConnection.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ClientVpnConnection.to_query v
let to_json v = `List (List.map ClientVpnConnection.to_json v)
let of_json j = Aws.Json.to_list ClientVpnConnection.of_json j
