type t = InstanceStateChange.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceStateChange.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceStateChange.to_query v
let to_json v = `List (List.map InstanceStateChange.to_json v)
let of_json j = Aws.Json.to_list InstanceStateChange.of_json j
