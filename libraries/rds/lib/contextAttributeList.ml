type t = ContextAttribute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ContextAttribute.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ContextAttribute.to_query v
let to_json v = `List (List.map ContextAttribute.to_json v)
let of_json j = Aws.Json.to_list ContextAttribute.of_json j
