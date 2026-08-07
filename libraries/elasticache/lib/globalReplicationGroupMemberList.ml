type t = GlobalReplicationGroupMember.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       GlobalReplicationGroupMember.parse
       (Aws.Xml.members "GlobalReplicationGroupMember" xml))

let to_query v = Aws.Query.to_query_list GlobalReplicationGroupMember.to_query v
let to_json v = `List (List.map GlobalReplicationGroupMember.to_json v)
let of_json j = Aws.Json.to_list GlobalReplicationGroupMember.of_json j
