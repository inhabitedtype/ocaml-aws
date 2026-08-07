type t = SpotInstanceRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SpotInstanceRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SpotInstanceRequest.to_query v
let to_json v = `List (List.map SpotInstanceRequest.to_json v)
let of_json j = Aws.Json.to_list SpotInstanceRequest.of_json j
