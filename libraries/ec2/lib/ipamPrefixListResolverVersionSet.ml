type t = IpamPrefixListResolverVersion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamPrefixListResolverVersion.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPrefixListResolverVersion.to_query v
let to_json v = `List (List.map IpamPrefixListResolverVersion.to_json v)
let of_json j = Aws.Json.to_list IpamPrefixListResolverVersion.of_json j
