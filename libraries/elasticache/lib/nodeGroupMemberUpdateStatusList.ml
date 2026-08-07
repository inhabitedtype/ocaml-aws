type t = NodeGroupMemberUpdateStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       NodeGroupMemberUpdateStatus.parse
       (Aws.Xml.members "NodeGroupMemberUpdateStatus" xml))

let to_query v = Aws.Query.to_query_list NodeGroupMemberUpdateStatus.to_query v
let to_json v = `List (List.map NodeGroupMemberUpdateStatus.to_json v)
let of_json j = Aws.Json.to_list NodeGroupMemberUpdateStatus.of_json j
