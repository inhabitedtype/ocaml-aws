type t = MetricValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MetricValue.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list MetricValue.to_query v
let to_json v = `List (List.map MetricValue.to_json v)
let of_json j = Aws.Json.to_list MetricValue.of_json j
