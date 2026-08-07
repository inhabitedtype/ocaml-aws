type t = DBSnapshotAttribute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DBSnapshotAttribute.parse (Aws.Xml.members "DBSnapshotAttribute" xml))

let to_query v = Aws.Query.to_query_list DBSnapshotAttribute.to_query v
let to_json v = `List (List.map DBSnapshotAttribute.to_json v)
let of_json j = Aws.Json.to_list DBSnapshotAttribute.of_json j
