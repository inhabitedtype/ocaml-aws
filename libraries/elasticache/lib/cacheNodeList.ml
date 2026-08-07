type t = CacheNode.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CacheNode.parse (Aws.Xml.members "CacheNode" xml))

let to_query v = Aws.Query.to_query_list CacheNode.to_query v
let to_json v = `List (List.map CacheNode.to_json v)
let of_json j = Aws.Json.to_list CacheNode.of_json j
