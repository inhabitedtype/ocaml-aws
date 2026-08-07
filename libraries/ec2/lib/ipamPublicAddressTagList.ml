type t = IpamPublicAddressTag.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IpamPublicAddressTag.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPublicAddressTag.to_query v
let to_json v = `List (List.map IpamPublicAddressTag.to_json v)
let of_json j = Aws.Json.to_list IpamPublicAddressTag.of_json j
