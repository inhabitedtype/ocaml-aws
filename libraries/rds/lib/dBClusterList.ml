type t = DBCluster.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DBCluster.parse (Aws.Xml.members "DBCluster" xml))

let to_query v = Aws.Query.to_query_list DBCluster.to_query v
let to_json v = `List (List.map DBCluster.to_json v)
let of_json j = Aws.Json.to_list DBCluster.of_json j
