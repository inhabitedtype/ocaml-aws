type t = GrantListEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map GrantListEntry.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list GrantListEntry.to_query v
let to_json v = `List (List.map GrantListEntry.to_json v)
let of_json j = Aws.Json.to_list GrantListEntry.of_json j
