type t = NodeGroupMember.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map NodeGroupMember.parse (Aws.Xml.members "NodeGroupMember" xml))

let to_query v = Aws.Query.to_query_list NodeGroupMember.to_query v
let to_json v = `List (List.map NodeGroupMember.to_json v)
let of_json j = Aws.Json.to_list NodeGroupMember.of_json j
