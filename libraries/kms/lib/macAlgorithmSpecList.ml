type t = MacAlgorithmSpec.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MacAlgorithmSpec.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list MacAlgorithmSpec.to_query v
let to_json v = `List (List.map MacAlgorithmSpec.to_json v)
let of_json j = Aws.Json.to_list MacAlgorithmSpec.of_json j
