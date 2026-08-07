type t = VpnTunnelOptionsSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map VpnTunnelOptionsSpecification.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list VpnTunnelOptionsSpecification.to_query v
let to_json v = `List (List.map VpnTunnelOptionsSpecification.to_json v)
let of_json j = Aws.Json.to_list VpnTunnelOptionsSpecification.of_json j
