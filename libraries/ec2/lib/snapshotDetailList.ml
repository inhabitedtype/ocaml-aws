type t = SnapshotDetail.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SnapshotDetail.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SnapshotDetail.to_query v
let to_json v = `List (List.map SnapshotDetail.to_json v)
let of_json j = Aws.Json.to_list SnapshotDetail.of_json j
