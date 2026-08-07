type t = MetricDataResult.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MetricDataResult.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list MetricDataResult.to_query v
let to_json v = `List (List.map MetricDataResult.to_json v)
let of_json j = Aws.Json.to_list MetricDataResult.of_json j
