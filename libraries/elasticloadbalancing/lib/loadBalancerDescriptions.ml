type t = LoadBalancerDescription.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map LoadBalancerDescription.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list LoadBalancerDescription.to_query v
let to_json v = `List (List.map LoadBalancerDescription.to_json v)
let of_json j = Aws.Json.to_list LoadBalancerDescription.of_json j
