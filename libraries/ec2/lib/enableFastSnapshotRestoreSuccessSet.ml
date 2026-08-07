type t = EnableFastSnapshotRestoreSuccessItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map EnableFastSnapshotRestoreSuccessItem.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list EnableFastSnapshotRestoreSuccessItem.to_query v
let to_json v = `List (List.map EnableFastSnapshotRestoreSuccessItem.to_json v)
let of_json j = Aws.Json.to_list EnableFastSnapshotRestoreSuccessItem.of_json j
