type t = DBSecurityGroupMembership.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DBSecurityGroupMembership.parse (Aws.Xml.members "DBSecurityGroup" xml))

let to_query v = Aws.Query.to_query_list DBSecurityGroupMembership.to_query v
let to_json v = `List (List.map DBSecurityGroupMembership.to_json v)
let of_json j = Aws.Json.to_list DBSecurityGroupMembership.of_json j
