type t = SecondaryInterfaceIpv4Address.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SecondaryInterfaceIpv4Address.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SecondaryInterfaceIpv4Address.to_query v
let to_json v = `List (List.map SecondaryInterfaceIpv4Address.to_json v)
let of_json j = Aws.Json.to_list SecondaryInterfaceIpv4Address.of_json j
