type t = EventCategoriesMap.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map EventCategoriesMap.parse (Aws.Xml.members "EventCategoriesMap" xml))

let to_query v = Aws.Query.to_query_list EventCategoriesMap.to_query v
let to_json v = `List (List.map EventCategoriesMap.to_json v)
let of_json j = Aws.Json.to_list EventCategoriesMap.of_json j
