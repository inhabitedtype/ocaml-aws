type t = ClientVpnAuthentication.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ClientVpnAuthentication.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ClientVpnAuthentication.to_query v
let to_json v = `List (List.map ClientVpnAuthentication.to_json v)
let of_json j = Aws.Json.to_list ClientVpnAuthentication.of_json j
