type t = Phase2DHGroupNumbersRequestListValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map Phase2DHGroupNumbersRequestListValue.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list Phase2DHGroupNumbersRequestListValue.to_query v
let to_json v = `List (List.map Phase2DHGroupNumbersRequestListValue.to_json v)
let of_json j = Aws.Json.to_list Phase2DHGroupNumbersRequestListValue.of_json j
