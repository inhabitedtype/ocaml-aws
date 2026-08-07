type t = DoubleRange.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DoubleRange.parse (Aws.Xml.members "DoubleRange" xml))

let to_query v = Aws.Query.to_query_list DoubleRange.to_query v
let to_json v = `List (List.map DoubleRange.to_json v)
let of_json j = Aws.Json.to_list DoubleRange.of_json j
