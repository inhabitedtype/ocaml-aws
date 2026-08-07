type t = SecondarySubnetIpv4CidrBlockAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SecondarySubnetIpv4CidrBlockAssociation.parse (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list SecondarySubnetIpv4CidrBlockAssociation.to_query v

let to_json v = `List (List.map SecondarySubnetIpv4CidrBlockAssociation.to_json v)
let of_json j = Aws.Json.to_list SecondarySubnetIpv4CidrBlockAssociation.of_json j
