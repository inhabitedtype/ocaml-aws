type t = SecondarySubnet.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SecondarySubnet.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SecondarySubnet.to_query v
let to_json v = `List (List.map SecondarySubnet.to_json v)
let of_json j = Aws.Json.to_list SecondarySubnet.of_json j
