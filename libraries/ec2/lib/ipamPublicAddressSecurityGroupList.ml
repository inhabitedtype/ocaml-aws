type t = IpamPublicAddressSecurityGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamPublicAddressSecurityGroup.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPublicAddressSecurityGroup.to_query v
let to_json v = `List (List.map IpamPublicAddressSecurityGroup.to_json v)
let of_json j = Aws.Json.to_list IpamPublicAddressSecurityGroup.of_json j
