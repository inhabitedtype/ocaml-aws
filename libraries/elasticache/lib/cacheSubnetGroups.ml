type t = CacheSubnetGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CacheSubnetGroup.parse (Aws.Xml.members "CacheSubnetGroup" xml))

let to_query v = Aws.Query.to_query_list CacheSubnetGroup.to_query v
let to_json v = `List (List.map CacheSubnetGroup.to_json v)
let of_json j = Aws.Json.to_list CacheSubnetGroup.of_json j
