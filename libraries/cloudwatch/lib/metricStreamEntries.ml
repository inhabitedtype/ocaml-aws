type t = MetricStreamEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MetricStreamEntry.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list MetricStreamEntry.to_query v
let to_json v = `List (List.map MetricStreamEntry.to_json v)
let of_json j = Aws.Json.to_list MetricStreamEntry.of_json j
