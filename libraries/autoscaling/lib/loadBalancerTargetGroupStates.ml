type t = LoadBalancerTargetGroupState.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map LoadBalancerTargetGroupState.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list LoadBalancerTargetGroupState.to_query v
let to_json v = `List (List.map LoadBalancerTargetGroupState.to_json v)
let of_json j = Aws.Json.to_list LoadBalancerTargetGroupState.of_json j
