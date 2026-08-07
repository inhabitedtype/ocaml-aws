type t = ManagedRule.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ManagedRule.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ManagedRule.to_query v
let to_json v = `List (List.map ManagedRule.to_json v)
let of_json j = Aws.Json.to_list ManagedRule.of_json j
