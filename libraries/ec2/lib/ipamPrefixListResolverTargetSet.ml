type t = IpamPrefixListResolverTarget.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamPrefixListResolverTarget.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPrefixListResolverTarget.to_query v
let to_json v = `List (List.map IpamPrefixListResolverTarget.to_json v)
let of_json j = Aws.Json.to_list IpamPrefixListResolverTarget.of_json j
