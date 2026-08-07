type t = MetricStreamFilter.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MetricStreamFilter.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list MetricStreamFilter.to_query v
let to_json v = `List (List.map MetricStreamFilter.to_json v)
let of_json j = Aws.Json.to_list MetricStreamFilter.of_json j
