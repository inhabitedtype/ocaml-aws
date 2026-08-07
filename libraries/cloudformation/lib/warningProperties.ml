type t = WarningProperty.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map WarningProperty.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list WarningProperty.to_query v
let to_json v = `List (List.map WarningProperty.to_json v)
let of_json j = Aws.Json.to_list WarningProperty.of_json j
