type t = IntegrationError.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IntegrationError.parse (Aws.Xml.members "IntegrationError" xml))

let to_query v = Aws.Query.to_query_list IntegrationError.to_query v
let to_json v = `List (List.map IntegrationError.to_json v)
let of_json j = Aws.Json.to_list IntegrationError.of_json j
