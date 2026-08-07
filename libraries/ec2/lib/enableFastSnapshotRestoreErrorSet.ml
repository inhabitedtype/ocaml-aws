type t = EnableFastSnapshotRestoreErrorItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map EnableFastSnapshotRestoreErrorItem.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list EnableFastSnapshotRestoreErrorItem.to_query v
let to_json v = `List (List.map EnableFastSnapshotRestoreErrorItem.to_json v)
let of_json j = Aws.Json.to_list EnableFastSnapshotRestoreErrorItem.of_json j
