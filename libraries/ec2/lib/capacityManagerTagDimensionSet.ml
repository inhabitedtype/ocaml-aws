type t = CapacityManagerTagDimension.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CapacityManagerTagDimension.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityManagerTagDimension.to_query v
let to_json v = `List (List.map CapacityManagerTagDimension.to_json v)
let of_json j = Aws.Json.to_list CapacityManagerTagDimension.of_json j
