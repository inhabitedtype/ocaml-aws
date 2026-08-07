type t = QueryResultColumn.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map QueryResultColumn.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list QueryResultColumn.to_query v
let to_json v = `List (List.map QueryResultColumn.to_json v)
let of_json j = Aws.Json.to_list QueryResultColumn.of_json j
