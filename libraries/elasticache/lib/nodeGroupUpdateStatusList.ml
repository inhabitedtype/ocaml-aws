type t = NodeGroupUpdateStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map NodeGroupUpdateStatus.parse (Aws.Xml.members "NodeGroupUpdateStatus" xml))

let to_query v = Aws.Query.to_query_list NodeGroupUpdateStatus.to_query v
let to_json v = `List (List.map NodeGroupUpdateStatus.to_json v)
let of_json j = Aws.Json.to_list NodeGroupUpdateStatus.of_json j
