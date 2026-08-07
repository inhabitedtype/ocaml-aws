type t = DBClusterSnapshotAttribute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       DBClusterSnapshotAttribute.parse
       (Aws.Xml.members "DBClusterSnapshotAttribute" xml))

let to_query v = Aws.Query.to_query_list DBClusterSnapshotAttribute.to_query v
let to_json v = `List (List.map DBClusterSnapshotAttribute.to_json v)
let of_json j = Aws.Json.to_list DBClusterSnapshotAttribute.of_json j
