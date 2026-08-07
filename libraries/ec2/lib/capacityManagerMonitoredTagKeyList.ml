type t = CapacityManagerMonitoredTagKey.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CapacityManagerMonitoredTagKey.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityManagerMonitoredTagKey.to_query v
let to_json v = `List (List.map CapacityManagerMonitoredTagKey.to_json v)
let of_json j = Aws.Json.to_list CapacityManagerMonitoredTagKey.of_json j
