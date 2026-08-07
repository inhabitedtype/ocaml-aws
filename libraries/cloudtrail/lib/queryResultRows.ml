type t = QueryResultRow.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map QueryResultRow.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list QueryResultRow.to_query v
let to_json v = `List (List.map QueryResultRow.to_json v)
let of_json j = Aws.Json.to_list QueryResultRow.of_json j
