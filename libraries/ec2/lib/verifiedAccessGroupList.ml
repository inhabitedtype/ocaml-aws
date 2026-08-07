type t = VerifiedAccessGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VerifiedAccessGroup.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VerifiedAccessGroup.to_query v
let to_json v = `List (List.map VerifiedAccessGroup.to_json v)
let of_json j = Aws.Json.to_list VerifiedAccessGroup.of_json j
