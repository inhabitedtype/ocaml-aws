type t = RevokedSecurityGroupRule.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map RevokedSecurityGroupRule.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RevokedSecurityGroupRule.to_query v
let to_json v = `List (List.map RevokedSecurityGroupRule.to_json v)
let of_json j = Aws.Json.to_list RevokedSecurityGroupRule.of_json j
