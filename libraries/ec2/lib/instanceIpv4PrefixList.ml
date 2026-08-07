type t = InstanceIpv4Prefix.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceIpv4Prefix.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceIpv4Prefix.to_query v
let to_json v = `List (List.map InstanceIpv4Prefix.to_json v)
let of_json j = Aws.Json.to_list InstanceIpv4Prefix.of_json j
