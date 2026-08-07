type t = TrafficMirrorSessionField.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TrafficMirrorSessionField.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list TrafficMirrorSessionField.to_query v
let to_json v = `List (List.map TrafficMirrorSessionField.to_json v)
let of_json j = Aws.Json.to_list TrafficMirrorSessionField.of_json j
