type t = ClientVpnRoute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ClientVpnRoute.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ClientVpnRoute.to_query v
let to_json v = `List (List.map ClientVpnRoute.to_json v)
let of_json j = Aws.Json.to_list ClientVpnRoute.of_json j
