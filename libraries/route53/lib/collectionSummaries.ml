type t = CollectionSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CollectionSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list CollectionSummary.to_query v
let to_json v = `List (List.map CollectionSummary.to_json v)
let of_json j = Aws.Json.to_list CollectionSummary.of_json j
