type t = IpamPolicyAllocationRuleRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamPolicyAllocationRuleRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPolicyAllocationRuleRequest.to_query v
let to_json v = `List (List.map IpamPolicyAllocationRuleRequest.to_json v)
let of_json j = Aws.Json.to_list IpamPolicyAllocationRuleRequest.of_json j
