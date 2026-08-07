type t = InstanceMonitoring.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceMonitoring.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceMonitoring.to_query v
let to_json v = `List (List.map InstanceMonitoring.to_json v)
let of_json j = Aws.Json.to_list InstanceMonitoring.of_json j
