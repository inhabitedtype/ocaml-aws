type t = WarningDetail.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map WarningDetail.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list WarningDetail.to_query v
let to_json v = `List (List.map WarningDetail.to_json v)
let of_json j = Aws.Json.to_list WarningDetail.of_json j
