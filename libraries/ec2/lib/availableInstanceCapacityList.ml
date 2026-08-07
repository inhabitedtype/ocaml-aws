type t = InstanceCapacity.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceCapacity.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceCapacity.to_query v
let to_json v = `List (List.map InstanceCapacity.to_json v)
let of_json j = Aws.Json.to_list InstanceCapacity.of_json j
