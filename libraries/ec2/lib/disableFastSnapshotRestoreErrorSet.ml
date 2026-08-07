type t = DisableFastSnapshotRestoreErrorItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DisableFastSnapshotRestoreErrorItem.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list DisableFastSnapshotRestoreErrorItem.to_query v
let to_json v = `List (List.map DisableFastSnapshotRestoreErrorItem.to_json v)
let of_json j = Aws.Json.to_list DisableFastSnapshotRestoreErrorItem.of_json j
