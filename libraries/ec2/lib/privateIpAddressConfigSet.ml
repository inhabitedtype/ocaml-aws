type t = ScheduledInstancesPrivateIpAddressConfig.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       ScheduledInstancesPrivateIpAddressConfig.parse
       (Aws.Xml.members "PrivateIpAddressConfigSet" xml))

let to_query v =
  Aws.Query.to_query_list ScheduledInstancesPrivateIpAddressConfig.to_query v

let to_json v = `List (List.map ScheduledInstancesPrivateIpAddressConfig.to_json v)
let of_json j = Aws.Json.to_list ScheduledInstancesPrivateIpAddressConfig.of_json j
