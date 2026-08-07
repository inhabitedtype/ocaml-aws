type t = VerifiedAccessTrustProviderCondensed.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map VerifiedAccessTrustProviderCondensed.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VerifiedAccessTrustProviderCondensed.to_query v
let to_json v = `List (List.map VerifiedAccessTrustProviderCondensed.to_json v)
let of_json j = Aws.Json.to_list VerifiedAccessTrustProviderCondensed.of_json j
