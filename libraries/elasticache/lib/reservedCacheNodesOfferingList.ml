type t = ReservedCacheNodesOffering.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       ReservedCacheNodesOffering.parse
       (Aws.Xml.members "ReservedCacheNodesOffering" xml))

let to_query v = Aws.Query.to_query_list ReservedCacheNodesOffering.to_query v
let to_json v = `List (List.map ReservedCacheNodesOffering.to_json v)
let of_json j = Aws.Json.to_list ReservedCacheNodesOffering.of_json j
