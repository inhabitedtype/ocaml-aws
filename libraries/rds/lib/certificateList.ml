type t = Certificate.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Certificate.parse (Aws.Xml.members "Certificate" xml))

let to_query v = Aws.Query.to_query_list Certificate.to_query v
let to_json v = `List (List.map Certificate.to_json v)
let of_json j = Aws.Json.to_list Certificate.of_json j
