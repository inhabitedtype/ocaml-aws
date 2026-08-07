type t = CancellationTerms.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CancellationTerms.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CancellationTerms.to_query v
let to_json v = `List (List.map CancellationTerms.to_json v)
let of_json j = Aws.Json.to_list CancellationTerms.of_json j
