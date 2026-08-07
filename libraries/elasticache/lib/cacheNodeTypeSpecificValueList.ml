type t = CacheNodeTypeSpecificValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       CacheNodeTypeSpecificValue.parse
       (Aws.Xml.members "CacheNodeTypeSpecificValue" xml))

let to_query v = Aws.Query.to_query_list CacheNodeTypeSpecificValue.to_query v
let to_json v = `List (List.map CacheNodeTypeSpecificValue.to_json v)
let of_json j = Aws.Json.to_list CacheNodeTypeSpecificValue.of_json j
