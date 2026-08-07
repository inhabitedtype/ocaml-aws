type t = DBClusterOptionGroupStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       DBClusterOptionGroupStatus.parse
       (Aws.Xml.members "DBClusterOptionGroup" xml))

let to_query v = Aws.Query.to_query_list DBClusterOptionGroupStatus.to_query v
let to_json v = `List (List.map DBClusterOptionGroupStatus.to_json v)
let of_json j = Aws.Json.to_list DBClusterOptionGroupStatus.of_json j
