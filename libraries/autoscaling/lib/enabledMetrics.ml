type t = EnabledMetric.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map EnabledMetric.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list EnabledMetric.to_query v
let to_json v = `List (List.map EnabledMetric.to_json v)
let of_json j = Aws.Json.to_list EnabledMetric.of_json j
