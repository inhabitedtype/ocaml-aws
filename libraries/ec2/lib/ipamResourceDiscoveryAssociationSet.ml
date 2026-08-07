type t = IpamResourceDiscoveryAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamResourceDiscoveryAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamResourceDiscoveryAssociation.to_query v
let to_json v = `List (List.map IpamResourceDiscoveryAssociation.to_json v)
let of_json j = Aws.Json.to_list IpamResourceDiscoveryAssociation.of_json j
