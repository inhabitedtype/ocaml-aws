type t = SecurityGroupMembership.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SecurityGroupMembership.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list SecurityGroupMembership.to_query v
let to_json v = `List (List.map SecurityGroupMembership.to_json v)
let of_json j = Aws.Json.to_list SecurityGroupMembership.of_json j
