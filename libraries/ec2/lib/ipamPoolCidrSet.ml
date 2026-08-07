type t = IpamPoolCidr.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IpamPoolCidr.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPoolCidr.to_query v
let to_json v = `List (List.map IpamPoolCidr.to_json v)
let of_json j = Aws.Json.to_list IpamPoolCidr.of_json j
