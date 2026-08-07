type t = SecurityGroupRule.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SecurityGroupRule.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SecurityGroupRule.to_query v
let to_json v = `List (List.map SecurityGroupRule.to_json v)
let of_json j = Aws.Json.to_list SecurityGroupRule.of_json j
