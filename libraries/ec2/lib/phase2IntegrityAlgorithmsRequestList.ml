type t = Phase2IntegrityAlgorithmsRequestListValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       Phase2IntegrityAlgorithmsRequestListValue.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list Phase2IntegrityAlgorithmsRequestListValue.to_query v

let to_json v = `List (List.map Phase2IntegrityAlgorithmsRequestListValue.to_json v)
let of_json j = Aws.Json.to_list Phase2IntegrityAlgorithmsRequestListValue.of_json j
