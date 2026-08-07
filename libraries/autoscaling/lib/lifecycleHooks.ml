type t = LifecycleHook.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LifecycleHook.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list LifecycleHook.to_query v
let to_json v = `List (List.map LifecycleHook.to_json v)
let of_json j = Aws.Json.to_list LifecycleHook.of_json j
