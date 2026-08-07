type t = LifecycleHookSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map LifecycleHookSpecification.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list LifecycleHookSpecification.to_query v
let to_json v = `List (List.map LifecycleHookSpecification.to_json v)
let of_json j = Aws.Json.to_list LifecycleHookSpecification.of_json j
