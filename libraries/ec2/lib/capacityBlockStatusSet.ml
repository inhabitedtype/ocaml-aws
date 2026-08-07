type t = CapacityBlockStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CapacityBlockStatus.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityBlockStatus.to_query v
let to_json v = `List (List.map CapacityBlockStatus.to_json v)
let of_json j = Aws.Json.to_list CapacityBlockStatus.of_json j
