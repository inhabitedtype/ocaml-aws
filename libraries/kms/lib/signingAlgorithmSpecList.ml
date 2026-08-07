type t = SigningAlgorithmSpec.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SigningAlgorithmSpec.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list SigningAlgorithmSpec.to_query v
let to_json v = `List (List.map SigningAlgorithmSpec.to_json v)
let of_json j = Aws.Json.to_list SigningAlgorithmSpec.of_json j
