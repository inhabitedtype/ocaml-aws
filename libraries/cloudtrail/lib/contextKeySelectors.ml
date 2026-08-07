type t = ContextKeySelector.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ContextKeySelector.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ContextKeySelector.to_query v
let to_json v = `List (List.map ContextKeySelector.to_json v)
let of_json j = Aws.Json.to_list ContextKeySelector.of_json j
