type t = MetricDimension.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MetricDimension.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list MetricDimension.to_query v
let to_json v = `List (List.map MetricDimension.to_json v)
let of_json j = Aws.Json.to_list MetricDimension.of_json j
