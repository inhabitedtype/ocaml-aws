type t = ClassicLoadBalancer.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ClassicLoadBalancer.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ClassicLoadBalancer.to_query v
let to_json v = `List (List.map ClassicLoadBalancer.to_json v)
let of_json j = Aws.Json.to_list ClassicLoadBalancer.of_json j
