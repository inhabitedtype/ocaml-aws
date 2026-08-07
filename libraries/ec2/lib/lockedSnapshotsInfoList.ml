type t = LockedSnapshotsInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LockedSnapshotsInfo.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LockedSnapshotsInfo.to_query v
let to_json v = `List (List.map LockedSnapshotsInfo.to_json v)
let of_json j = Aws.Json.to_list LockedSnapshotsInfo.of_json j
