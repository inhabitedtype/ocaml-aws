type t = ReservedCacheNode.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ReservedCacheNode.parse (Aws.Xml.members "ReservedCacheNode" xml))

let to_query v = Aws.Query.to_query_list ReservedCacheNode.to_query v
let to_json v = `List (List.map ReservedCacheNode.to_json v)
let of_json j = Aws.Json.to_list ReservedCacheNode.of_json j
