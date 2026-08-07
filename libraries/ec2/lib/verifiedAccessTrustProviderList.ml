type t = VerifiedAccessTrustProvider.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map VerifiedAccessTrustProvider.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VerifiedAccessTrustProvider.to_query v
let to_json v = `List (List.map VerifiedAccessTrustProvider.to_json v)
let of_json j = Aws.Json.to_list VerifiedAccessTrustProvider.of_json j
