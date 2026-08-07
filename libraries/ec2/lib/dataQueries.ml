type t = DataQuery.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DataQuery.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list DataQuery.to_query v
let to_json v = `List (List.map DataQuery.to_json v)
let of_json j = Aws.Json.to_list DataQuery.of_json j
