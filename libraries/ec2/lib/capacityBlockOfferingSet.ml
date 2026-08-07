type t = CapacityBlockOffering.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CapacityBlockOffering.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityBlockOffering.to_query v
let to_json v = `List (List.map CapacityBlockOffering.to_json v)
let of_json j = Aws.Json.to_list CapacityBlockOffering.of_json j
