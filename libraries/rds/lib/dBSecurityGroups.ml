type t = DBSecurityGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DBSecurityGroup.parse (Aws.Xml.members "DBSecurityGroup" xml))

let to_query v = Aws.Query.to_query_list DBSecurityGroup.to_query v
let to_json v = `List (List.map DBSecurityGroup.to_json v)
let of_json j = Aws.Json.to_list DBSecurityGroup.of_json j
