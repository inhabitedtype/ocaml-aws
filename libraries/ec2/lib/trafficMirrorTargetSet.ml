type t = TrafficMirrorTarget.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TrafficMirrorTarget.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TrafficMirrorTarget.to_query v
let to_json v = `List (List.map TrafficMirrorTarget.to_json v)
let of_json j = Aws.Json.to_list TrafficMirrorTarget.of_json j
