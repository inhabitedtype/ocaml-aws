type t = Phase1DHGroupNumbersListValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map Phase1DHGroupNumbersListValue.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list Phase1DHGroupNumbersListValue.to_query v
let to_json v = `List (List.map Phase1DHGroupNumbersListValue.to_json v)
let of_json j = Aws.Json.to_list Phase1DHGroupNumbersListValue.of_json j
