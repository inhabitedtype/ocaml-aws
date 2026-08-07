type t = GlobalClusterMember.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map GlobalClusterMember.parse (Aws.Xml.members "GlobalClusterMember" xml))

let to_query v = Aws.Query.to_query_list GlobalClusterMember.to_query v
let to_json v = `List (List.map GlobalClusterMember.to_json v)
let of_json j = Aws.Json.to_list GlobalClusterMember.of_json j
