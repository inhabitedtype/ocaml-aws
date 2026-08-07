type t = ParameterNameValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ParameterNameValue.parse (Aws.Xml.members "ParameterNameValue" xml))

let to_query v = Aws.Query.to_query_list ParameterNameValue.to_query v
let to_json v = `List (List.map ParameterNameValue.to_json v)
let of_json j = Aws.Json.to_list ParameterNameValue.of_json j
