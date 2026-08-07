type t = ChangeSetHook.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ChangeSetHook.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ChangeSetHook.to_query v
let to_json v = `List (List.map ChangeSetHook.to_json v)
let of_json j = Aws.Json.to_list ChangeSetHook.of_json j
