type t = Ipv6CidrBlock.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Ipv6CidrBlock.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list Ipv6CidrBlock.to_query v
let to_json v = `List (List.map Ipv6CidrBlock.to_json v)
let of_json j = Aws.Json.to_list Ipv6CidrBlock.of_json j
