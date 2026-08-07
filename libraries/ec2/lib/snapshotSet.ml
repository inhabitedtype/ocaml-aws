type t = SnapshotInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SnapshotInfo.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SnapshotInfo.to_query v
let to_json v = `List (List.map SnapshotInfo.to_json v)
let of_json j = Aws.Json.to_list SnapshotInfo.of_json j
