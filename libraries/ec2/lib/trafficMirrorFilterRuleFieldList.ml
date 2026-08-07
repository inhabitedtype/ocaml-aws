type t = TrafficMirrorFilterRuleField.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TrafficMirrorFilterRuleField.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list TrafficMirrorFilterRuleField.to_query v
let to_json v = `List (List.map TrafficMirrorFilterRuleField.to_json v)
let of_json j = Aws.Json.to_list TrafficMirrorFilterRuleField.of_json j
