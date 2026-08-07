type t = AdvancedFieldSelector.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AdvancedFieldSelector.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AdvancedFieldSelector.to_query v
let to_json v = `List (List.map AdvancedFieldSelector.to_json v)
let of_json j = Aws.Json.to_list AdvancedFieldSelector.of_json j
