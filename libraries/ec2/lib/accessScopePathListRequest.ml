type t = AccessScopePathRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AccessScopePathRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AccessScopePathRequest.to_query v
let to_json v = `List (List.map AccessScopePathRequest.to_json v)
let of_json j = Aws.Json.to_list AccessScopePathRequest.of_json j
