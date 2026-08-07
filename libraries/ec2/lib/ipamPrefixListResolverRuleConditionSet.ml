type t = IpamPrefixListResolverRuleCondition.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamPrefixListResolverRuleCondition.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPrefixListResolverRuleCondition.to_query v
let to_json v = `List (List.map IpamPrefixListResolverRuleCondition.to_json v)
let of_json j = Aws.Json.to_list IpamPrefixListResolverRuleCondition.of_json j
