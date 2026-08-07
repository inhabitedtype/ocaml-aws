type t = CacheCluster.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CacheCluster.parse (Aws.Xml.members "CacheCluster" xml))

let to_query v = Aws.Query.to_query_list CacheCluster.to_query v
let to_json v = `List (List.map CacheCluster.to_json v)
let of_json j = Aws.Json.to_list CacheCluster.of_json j
