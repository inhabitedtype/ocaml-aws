type t = InstanceState.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceState.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list InstanceState.to_query v
let to_json v = `List (List.map InstanceState.to_json v)
let of_json j = Aws.Json.to_list InstanceState.of_json j
