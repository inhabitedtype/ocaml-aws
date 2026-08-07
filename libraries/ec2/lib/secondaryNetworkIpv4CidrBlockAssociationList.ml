type t = SecondaryNetworkIpv4CidrBlockAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SecondaryNetworkIpv4CidrBlockAssociation.parse (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list SecondaryNetworkIpv4CidrBlockAssociation.to_query v

let to_json v = `List (List.map SecondaryNetworkIpv4CidrBlockAssociation.to_json v)
let of_json j = Aws.Json.to_list SecondaryNetworkIpv4CidrBlockAssociation.of_json j
