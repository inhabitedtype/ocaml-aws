type t = IpamPrefixListResolver.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IpamPrefixListResolver.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPrefixListResolver.to_query v
let to_json v = `List (List.map IpamPrefixListResolver.to_json v)
let of_json j = Aws.Json.to_list IpamPrefixListResolver.of_json j
