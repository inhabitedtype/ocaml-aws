type t = MetricReference.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MetricReference.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list MetricReference.to_query v
let to_json v = `List (List.map MetricReference.to_json v)
let of_json j = Aws.Json.to_list MetricReference.of_json j
