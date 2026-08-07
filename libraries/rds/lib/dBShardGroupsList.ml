type t = DBShardGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DBShardGroup.parse (Aws.Xml.members "DBShardGroup" xml))

let to_query v = Aws.Query.to_query_list DBShardGroup.to_query v
let to_json v = `List (List.map DBShardGroup.to_json v)
let of_json j = Aws.Json.to_list DBShardGroup.of_json j
