type t = DBClusterEndpoint.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DBClusterEndpoint.parse (Aws.Xml.members "DBClusterEndpointList" xml))

let to_query v = Aws.Query.to_query_list DBClusterEndpoint.to_query v
let to_json v = `List (List.map DBClusterEndpoint.to_json v)
let of_json j = Aws.Json.to_list DBClusterEndpoint.of_json j
