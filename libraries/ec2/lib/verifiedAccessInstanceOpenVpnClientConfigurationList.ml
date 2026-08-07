type t = VerifiedAccessInstanceOpenVpnClientConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       VerifiedAccessInstanceOpenVpnClientConfiguration.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list VerifiedAccessInstanceOpenVpnClientConfiguration.to_query v

let to_json v =
  `List (List.map VerifiedAccessInstanceOpenVpnClientConfiguration.to_json v)

let of_json j =
  Aws.Json.to_list VerifiedAccessInstanceOpenVpnClientConfiguration.of_json j
