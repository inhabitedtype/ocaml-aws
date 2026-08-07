type t = VpnConnectionDeviceType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map VpnConnectionDeviceType.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpnConnectionDeviceType.to_query v
let to_json v = `List (List.map VpnConnectionDeviceType.to_json v)
let of_json j = Aws.Json.to_list VpnConnectionDeviceType.of_json j
