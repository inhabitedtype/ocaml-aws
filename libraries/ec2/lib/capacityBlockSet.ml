type t = CapacityBlock.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CapacityBlock.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityBlock.to_query v
let to_json v = `List (List.map CapacityBlock.to_json v)
let of_json j = Aws.Json.to_list CapacityBlock.of_json j
