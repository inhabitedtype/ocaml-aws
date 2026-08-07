type t = ScheduledInstancesIpv6Address.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ScheduledInstancesIpv6Address.parse (Aws.Xml.members "Ipv6Address" xml))

let to_query v = Aws.Query.to_query_list ScheduledInstancesIpv6Address.to_query v
let to_json v = `List (List.map ScheduledInstancesIpv6Address.to_json v)
let of_json j = Aws.Json.to_list ScheduledInstancesIpv6Address.of_json j
