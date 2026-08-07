type t = SupportedEngineLifecycle.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       SupportedEngineLifecycle.parse
       (Aws.Xml.members "SupportedEngineLifecycle" xml))

let to_query v = Aws.Query.to_query_list SupportedEngineLifecycle.to_query v
let to_json v = `List (List.map SupportedEngineLifecycle.to_json v)
let of_json j = Aws.Json.to_list SupportedEngineLifecycle.of_json j
