type t = DBMajorEngineVersion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DBMajorEngineVersion.parse (Aws.Xml.members "DBMajorEngineVersion" xml))

let to_query v = Aws.Query.to_query_list DBMajorEngineVersion.to_query v
let to_json v = `List (List.map DBMajorEngineVersion.to_json v)
let of_json j = Aws.Json.to_list DBMajorEngineVersion.of_json j
