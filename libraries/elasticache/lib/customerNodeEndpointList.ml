type t = CustomerNodeEndpoint.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CustomerNodeEndpoint.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list CustomerNodeEndpoint.to_query v
let to_json v = `List (List.map CustomerNodeEndpoint.to_json v)
let of_json j = Aws.Json.to_list CustomerNodeEndpoint.of_json j
