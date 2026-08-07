type t = CacheEngineVersion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CacheEngineVersion.parse (Aws.Xml.members "CacheEngineVersion" xml))

let to_query v = Aws.Query.to_query_list CacheEngineVersion.to_query v
let to_json v = `List (List.map CacheEngineVersion.to_json v)
let of_json j = Aws.Json.to_list CacheEngineVersion.of_json j
