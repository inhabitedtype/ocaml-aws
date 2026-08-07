type t = CacheNodeTypeSpecificParameter.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       CacheNodeTypeSpecificParameter.parse
       (Aws.Xml.members "CacheNodeTypeSpecificParameter" xml))

let to_query v = Aws.Query.to_query_list CacheNodeTypeSpecificParameter.to_query v
let to_json v = `List (List.map CacheNodeTypeSpecificParameter.to_json v)
let of_json j = Aws.Json.to_list CacheNodeTypeSpecificParameter.of_json j
