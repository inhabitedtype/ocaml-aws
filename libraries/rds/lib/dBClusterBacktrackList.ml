type t = DBClusterBacktrack.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DBClusterBacktrack.parse (Aws.Xml.members "DBClusterBacktrack" xml))

let to_query v = Aws.Query.to_query_list DBClusterBacktrack.to_query v
let to_json v = `List (List.map DBClusterBacktrack.to_json v)
let of_json j = Aws.Json.to_list DBClusterBacktrack.of_json j
