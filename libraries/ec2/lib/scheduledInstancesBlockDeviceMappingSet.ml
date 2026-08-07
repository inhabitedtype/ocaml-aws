type t = ScheduledInstancesBlockDeviceMapping.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       ScheduledInstancesBlockDeviceMapping.parse
       (Aws.Xml.members "BlockDeviceMapping" xml))

let to_query v = Aws.Query.to_query_list ScheduledInstancesBlockDeviceMapping.to_query v
let to_json v = `List (List.map ScheduledInstancesBlockDeviceMapping.to_json v)
let of_json j = Aws.Json.to_list ScheduledInstancesBlockDeviceMapping.of_json j
