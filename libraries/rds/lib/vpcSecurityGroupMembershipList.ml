type t = VpcSecurityGroupMembership.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       VpcSecurityGroupMembership.parse
       (Aws.Xml.members "VpcSecurityGroupMembership" xml))

let to_query v = Aws.Query.to_query_list VpcSecurityGroupMembership.to_query v
let to_json v = `List (List.map VpcSecurityGroupMembership.to_json v)
let of_json j = Aws.Json.to_list VpcSecurityGroupMembership.of_json j
