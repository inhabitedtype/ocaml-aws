type t = DBClusterStatusInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DBClusterStatusInfo.parse (Aws.Xml.members "DBClusterStatusInfo" xml))

let to_query v = Aws.Query.to_query_list DBClusterStatusInfo.to_query v
let to_json v = `List (List.map DBClusterStatusInfo.to_json v)
let of_json j = Aws.Json.to_list DBClusterStatusInfo.of_json j
