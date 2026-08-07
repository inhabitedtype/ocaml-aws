type t = CapacityManagerDimension.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CapacityManagerDimension.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityManagerDimension.to_query v
let to_json v = `List (List.map CapacityManagerDimension.to_json v)
let of_json j = Aws.Json.to_list CapacityManagerDimension.of_json j
