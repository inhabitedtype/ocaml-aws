type t = SearchSampleQueriesSearchResult.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SearchSampleQueriesSearchResult.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list SearchSampleQueriesSearchResult.to_query v
let to_json v = `List (List.map SearchSampleQueriesSearchResult.to_json v)
let of_json j = Aws.Json.to_list SearchSampleQueriesSearchResult.of_json j
