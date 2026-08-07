type t = TrafficSourceState.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TrafficSourceState.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list TrafficSourceState.to_query v
let to_json v = `List (List.map TrafficSourceState.to_json v)
let of_json j = Aws.Json.to_list TrafficSourceState.of_json j
