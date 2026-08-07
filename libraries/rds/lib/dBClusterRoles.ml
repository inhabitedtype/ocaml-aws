type t = DBClusterRole.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DBClusterRole.parse (Aws.Xml.members "DBClusterRole" xml))

let to_query v = Aws.Query.to_query_list DBClusterRole.to_query v
let to_json v = `List (List.map DBClusterRole.to_json v)
let of_json j = Aws.Json.to_list DBClusterRole.of_json j
