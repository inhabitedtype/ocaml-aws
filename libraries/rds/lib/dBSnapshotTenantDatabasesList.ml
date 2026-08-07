type t = DBSnapshotTenantDatabase.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       DBSnapshotTenantDatabase.parse
       (Aws.Xml.members "DBSnapshotTenantDatabase" xml))

let to_query v = Aws.Query.to_query_list DBSnapshotTenantDatabase.to_query v
let to_json v = `List (List.map DBSnapshotTenantDatabase.to_json v)
let of_json j = Aws.Json.to_list DBSnapshotTenantDatabase.of_json j
