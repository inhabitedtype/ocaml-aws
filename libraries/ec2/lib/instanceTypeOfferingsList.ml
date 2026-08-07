type t = InstanceTypeOffering.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceTypeOffering.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceTypeOffering.to_query v
let to_json v = `List (List.map InstanceTypeOffering.to_json v)
let of_json j = Aws.Json.to_list InstanceTypeOffering.of_json j
