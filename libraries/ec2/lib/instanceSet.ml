type t = InstanceTopology.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceTopology.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceTopology.to_query v
let to_json v = `List (List.map InstanceTopology.to_json v)
let of_json j = Aws.Json.to_list InstanceTopology.of_json j
