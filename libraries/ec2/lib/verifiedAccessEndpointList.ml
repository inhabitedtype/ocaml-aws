type t = VerifiedAccessEndpoint.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VerifiedAccessEndpoint.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VerifiedAccessEndpoint.to_query v
let to_json v = `List (List.map VerifiedAccessEndpoint.to_json v)
let of_json j = Aws.Json.to_list VerifiedAccessEndpoint.of_json j
