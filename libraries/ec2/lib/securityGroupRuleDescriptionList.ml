type t = SecurityGroupRuleDescription.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SecurityGroupRuleDescription.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SecurityGroupRuleDescription.to_query v
let to_json v = `List (List.map SecurityGroupRuleDescription.to_json v)
let of_json j = Aws.Json.to_list SecurityGroupRuleDescription.of_json j
