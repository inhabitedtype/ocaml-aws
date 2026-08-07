type t = InstanceStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceStatus.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceStatus.to_query v
let to_json v = `List (List.map InstanceStatus.to_json v)
let of_json j = Aws.Json.to_list InstanceStatus.of_json j
