type t = ClientVpnEndpoint.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ClientVpnEndpoint.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ClientVpnEndpoint.to_query v
let to_json v = `List (List.map ClientVpnEndpoint.to_json v)
let of_json j = Aws.Json.to_list ClientVpnEndpoint.of_json j
