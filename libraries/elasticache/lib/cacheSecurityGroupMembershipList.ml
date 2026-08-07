type t = CacheSecurityGroupMembership.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       CacheSecurityGroupMembership.parse
       (Aws.Xml.members "CacheSecurityGroup" xml))

let to_query v = Aws.Query.to_query_list CacheSecurityGroupMembership.to_query v
let to_json v = `List (List.map CacheSecurityGroupMembership.to_json v)
let of_json j = Aws.Json.to_list CacheSecurityGroupMembership.of_json j
