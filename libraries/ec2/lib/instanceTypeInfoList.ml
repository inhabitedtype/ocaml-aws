type t = InstanceTypeInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceTypeInfo.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceTypeInfo.to_query v
let to_json v = `List (List.map InstanceTypeInfo.to_json v)
let of_json j = Aws.Json.to_list InstanceTypeInfo.of_json j
