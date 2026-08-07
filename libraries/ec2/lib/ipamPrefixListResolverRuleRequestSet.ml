type t = IpamPrefixListResolverRuleRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamPrefixListResolverRuleRequest.parse (Aws.Xml.members "Rule" xml))

let to_query v = Aws.Query.to_query_list IpamPrefixListResolverRuleRequest.to_query v
let to_json v = `List (List.map IpamPrefixListResolverRuleRequest.to_json v)
let of_json j = Aws.Json.to_list IpamPrefixListResolverRuleRequest.of_json j
