type t = DisableFastSnapshotRestoreStateErrorItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DisableFastSnapshotRestoreStateErrorItem.parse (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list DisableFastSnapshotRestoreStateErrorItem.to_query v

let to_json v = `List (List.map DisableFastSnapshotRestoreStateErrorItem.to_json v)
let of_json j = Aws.Json.to_list DisableFastSnapshotRestoreStateErrorItem.of_json j
