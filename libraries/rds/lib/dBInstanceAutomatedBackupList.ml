type t = DBInstanceAutomatedBackup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       DBInstanceAutomatedBackup.parse
       (Aws.Xml.members "DBInstanceAutomatedBackup" xml))

let to_query v = Aws.Query.to_query_list DBInstanceAutomatedBackup.to_query v
let to_json v = `List (List.map DBInstanceAutomatedBackup.to_json v)
let of_json j = Aws.Json.to_list DBInstanceAutomatedBackup.of_json j
