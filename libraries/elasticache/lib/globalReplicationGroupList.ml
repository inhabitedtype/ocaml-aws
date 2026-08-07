type t = GlobalReplicationGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map GlobalReplicationGroup.parse (Aws.Xml.members "GlobalReplicationGroup" xml))

let to_query v = Aws.Query.to_query_list GlobalReplicationGroup.to_query v
let to_json v = `List (List.map GlobalReplicationGroup.to_json v)
let of_json j = Aws.Json.to_list GlobalReplicationGroup.of_json j
