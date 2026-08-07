type t = TrafficPolicyInstance.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TrafficPolicyInstance.parse (Aws.Xml.members "TrafficPolicyInstance" xml))

let to_query v = Aws.Query.to_query_list TrafficPolicyInstance.to_query v
let to_json v = `List (List.map TrafficPolicyInstance.to_json v)
let of_json j = Aws.Json.to_list TrafficPolicyInstance.of_json j
