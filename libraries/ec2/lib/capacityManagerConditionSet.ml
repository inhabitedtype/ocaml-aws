type t = CapacityManagerCondition.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CapacityManagerCondition.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityManagerCondition.to_query v
let to_json v = `List (List.map CapacityManagerCondition.to_json v)
let of_json j = Aws.Json.to_list CapacityManagerCondition.of_json j
