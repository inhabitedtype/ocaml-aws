type t = TrafficPolicy.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TrafficPolicy.parse (Aws.Xml.members "TrafficPolicy" xml))

let to_query v = Aws.Query.to_query_list TrafficPolicy.to_query v
let to_json v = `List (List.map TrafficPolicy.to_json v)
let of_json j = Aws.Json.to_list TrafficPolicy.of_json j
