type t = MetricAlarm.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MetricAlarm.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list MetricAlarm.to_query v
let to_json v = `List (List.map MetricAlarm.to_json v)
let of_json j = Aws.Json.to_list MetricAlarm.of_json j
