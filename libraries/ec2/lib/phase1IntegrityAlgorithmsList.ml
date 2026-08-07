type t = Phase1IntegrityAlgorithmsListValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map Phase1IntegrityAlgorithmsListValue.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list Phase1IntegrityAlgorithmsListValue.to_query v
let to_json v = `List (List.map Phase1IntegrityAlgorithmsListValue.to_json v)
let of_json j = Aws.Json.to_list Phase1IntegrityAlgorithmsListValue.of_json j
