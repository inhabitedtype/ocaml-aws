type t = IpamPrefixListResolverRule.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamPrefixListResolverRule.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPrefixListResolverRule.to_query v
let to_json v = `List (List.map IpamPrefixListResolverRule.to_json v)
let of_json j = Aws.Json.to_list IpamPrefixListResolverRule.of_json j
