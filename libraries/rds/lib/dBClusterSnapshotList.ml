type t = DBClusterSnapshot.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DBClusterSnapshot.parse (Aws.Xml.members "DBClusterSnapshot" xml))

let to_query v = Aws.Query.to_query_list DBClusterSnapshot.to_query v
let to_json v = `List (List.map DBClusterSnapshot.to_json v)
let of_json j = Aws.Json.to_list DBClusterSnapshot.of_json j
