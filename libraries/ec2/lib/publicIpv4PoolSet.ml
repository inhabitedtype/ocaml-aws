type t = PublicIpv4Pool.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PublicIpv4Pool.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PublicIpv4Pool.to_query v
let to_json v = `List (List.map PublicIpv4Pool.to_json v)
let of_json j = Aws.Json.to_list PublicIpv4Pool.of_json j
