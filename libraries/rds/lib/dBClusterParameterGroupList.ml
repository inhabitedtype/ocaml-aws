type t = DBClusterParameterGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       DBClusterParameterGroup.parse
       (Aws.Xml.members "DBClusterParameterGroup" xml))

let to_query v = Aws.Query.to_query_list DBClusterParameterGroup.to_query v
let to_json v = `List (List.map DBClusterParameterGroup.to_json v)
let of_json j = Aws.Json.to_list DBClusterParameterGroup.of_json j
