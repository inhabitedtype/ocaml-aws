type t = CacheParameterGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CacheParameterGroup.parse (Aws.Xml.members "CacheParameterGroup" xml))

let to_query v = Aws.Query.to_query_list CacheParameterGroup.to_query v
let to_json v = `List (List.map CacheParameterGroup.to_json v)
let of_json j = Aws.Json.to_list CacheParameterGroup.of_json j
