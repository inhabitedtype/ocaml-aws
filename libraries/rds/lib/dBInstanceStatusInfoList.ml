type t = DBInstanceStatusInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DBInstanceStatusInfo.parse (Aws.Xml.members "DBInstanceStatusInfo" xml))

let to_query v = Aws.Query.to_query_list DBInstanceStatusInfo.to_query v
let to_json v = `List (List.map DBInstanceStatusInfo.to_json v)
let of_json j = Aws.Json.to_list DBInstanceStatusInfo.of_json j
