type t = LoadBalancerState.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LoadBalancerState.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list LoadBalancerState.to_query v
let to_json v = `List (List.map LoadBalancerState.to_json v)
let of_json j = Aws.Json.to_list LoadBalancerState.of_json j
