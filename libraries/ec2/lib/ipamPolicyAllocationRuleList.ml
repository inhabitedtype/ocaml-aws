type t = IpamPolicyAllocationRule.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamPolicyAllocationRule.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPolicyAllocationRule.to_query v
let to_json v = `List (List.map IpamPolicyAllocationRule.to_json v)
let of_json j = Aws.Json.to_list IpamPolicyAllocationRule.of_json j
