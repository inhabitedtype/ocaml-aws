type t = InstanceCount.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceCount.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceCount.to_query v
let to_json v = `List (List.map InstanceCount.to_json v)
let of_json j = Aws.Json.to_list InstanceCount.of_json j
