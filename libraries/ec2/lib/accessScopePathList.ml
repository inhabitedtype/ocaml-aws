type t = AccessScopePath.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AccessScopePath.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AccessScopePath.to_query v
let to_json v = `List (List.map AccessScopePath.to_json v)
let of_json j = Aws.Json.to_list AccessScopePath.of_json j
