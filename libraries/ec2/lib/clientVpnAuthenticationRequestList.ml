type t = ClientVpnAuthenticationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ClientVpnAuthenticationRequest.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ClientVpnAuthenticationRequest.to_query v
let to_json v = `List (List.map ClientVpnAuthenticationRequest.to_json v)
let of_json j = Aws.Json.to_list ClientVpnAuthenticationRequest.of_json j
