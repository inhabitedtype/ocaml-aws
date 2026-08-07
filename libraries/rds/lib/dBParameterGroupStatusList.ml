type t = DBParameterGroupStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DBParameterGroupStatus.parse (Aws.Xml.members "DBParameterGroup" xml))

let to_query v = Aws.Query.to_query_list DBParameterGroupStatus.to_query v
let to_json v = `List (List.map DBParameterGroupStatus.to_json v)
let of_json j = Aws.Json.to_list DBParameterGroupStatus.of_json j
