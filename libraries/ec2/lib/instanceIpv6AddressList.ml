type t = InstanceIpv6Address.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceIpv6Address.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceIpv6Address.to_query v
let to_json v = `List (List.map InstanceIpv6Address.to_json v)
let of_json j = Aws.Json.to_list InstanceIpv6Address.of_json j
