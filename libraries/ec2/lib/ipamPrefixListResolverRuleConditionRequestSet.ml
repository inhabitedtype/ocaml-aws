type t = IpamPrefixListResolverRuleConditionRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       IpamPrefixListResolverRuleConditionRequest.parse
       (Aws.Xml.members "Condition" xml))

let to_query v =
  Aws.Query.to_query_list IpamPrefixListResolverRuleConditionRequest.to_query v

let to_json v = `List (List.map IpamPrefixListResolverRuleConditionRequest.to_json v)
let of_json j = Aws.Json.to_list IpamPrefixListResolverRuleConditionRequest.of_json j
