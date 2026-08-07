type t = IpamPrefixListResolverVersionEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamPrefixListResolverVersionEntry.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPrefixListResolverVersionEntry.to_query v
let to_json v = `List (List.map IpamPrefixListResolverVersionEntry.to_json v)
let of_json j = Aws.Json.to_list IpamPrefixListResolverVersionEntry.of_json j
