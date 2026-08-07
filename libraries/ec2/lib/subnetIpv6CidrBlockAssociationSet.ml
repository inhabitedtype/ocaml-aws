type t = SubnetIpv6CidrBlockAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SubnetIpv6CidrBlockAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SubnetIpv6CidrBlockAssociation.to_query v
let to_json v = `List (List.map SubnetIpv6CidrBlockAssociation.to_json v)
let of_json j = Aws.Json.to_list SubnetIpv6CidrBlockAssociation.of_json j
