type t = GlobalCluster.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map GlobalCluster.parse (Aws.Xml.members "GlobalClusterMember" xml))

let to_query v = Aws.Query.to_query_list GlobalCluster.to_query v
let to_json v = `List (List.map GlobalCluster.to_json v)
let of_json j = Aws.Json.to_list GlobalCluster.of_json j
