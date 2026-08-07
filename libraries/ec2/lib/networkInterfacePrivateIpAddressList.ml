type t = NetworkInterfacePrivateIpAddress.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map NetworkInterfacePrivateIpAddress.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list NetworkInterfacePrivateIpAddress.to_query v
let to_json v = `List (List.map NetworkInterfacePrivateIpAddress.to_json v)
let of_json j = Aws.Json.to_list NetworkInterfacePrivateIpAddress.of_json j
