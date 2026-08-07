type t = DescribeFastSnapshotRestoreSuccessItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DescribeFastSnapshotRestoreSuccessItem.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list DescribeFastSnapshotRestoreSuccessItem.to_query v
let to_json v = `List (List.map DescribeFastSnapshotRestoreSuccessItem.to_json v)
let of_json j = Aws.Json.to_list DescribeFastSnapshotRestoreSuccessItem.of_json j
