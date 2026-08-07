type t = DBInstanceAutomatedBackupsReplication.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       DBInstanceAutomatedBackupsReplication.parse
       (Aws.Xml.members "DBInstanceAutomatedBackupsReplication" xml))

let to_query v = Aws.Query.to_query_list DBInstanceAutomatedBackupsReplication.to_query v
let to_json v = `List (List.map DBInstanceAutomatedBackupsReplication.to_json v)
let of_json j = Aws.Json.to_list DBInstanceAutomatedBackupsReplication.of_json j
