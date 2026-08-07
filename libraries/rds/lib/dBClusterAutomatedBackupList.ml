type t = DBClusterAutomatedBackup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       DBClusterAutomatedBackup.parse
       (Aws.Xml.members "DBClusterAutomatedBackup" xml))

let to_query v = Aws.Query.to_query_list DBClusterAutomatedBackup.to_query v
let to_json v = `List (List.map DBClusterAutomatedBackup.to_json v)
let of_json j = Aws.Json.to_list DBClusterAutomatedBackup.of_json j
