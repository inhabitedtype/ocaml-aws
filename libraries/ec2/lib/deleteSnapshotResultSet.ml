type t = DeleteSnapshotReturnCode.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DeleteSnapshotReturnCode.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list DeleteSnapshotReturnCode.to_query v
let to_json v = `List (List.map DeleteSnapshotReturnCode.to_json v)
let of_json j = Aws.Json.to_list DeleteSnapshotReturnCode.of_json j
