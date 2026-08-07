type t = ScheduledInstancesNetworkInterface.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       ScheduledInstancesNetworkInterface.parse
       (Aws.Xml.members "NetworkInterface" xml))

let to_query v = Aws.Query.to_query_list ScheduledInstancesNetworkInterface.to_query v
let to_json v = `List (List.map ScheduledInstancesNetworkInterface.to_json v)
let of_json j = Aws.Json.to_list ScheduledInstancesNetworkInterface.of_json j
