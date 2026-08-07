type t = DisableFastSnapshotRestoreSuccessItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DisableFastSnapshotRestoreSuccessItem.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list DisableFastSnapshotRestoreSuccessItem.to_query v
let to_json v = `List (List.map DisableFastSnapshotRestoreSuccessItem.to_json v)
let of_json j = Aws.Json.to_list DisableFastSnapshotRestoreSuccessItem.of_json j
