type t = VerifiedAccessInstanceOpenVpnClientConfigurationRoute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       VerifiedAccessInstanceOpenVpnClientConfigurationRoute.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list VerifiedAccessInstanceOpenVpnClientConfigurationRoute.to_query v

let to_json v =
  `List (List.map VerifiedAccessInstanceOpenVpnClientConfigurationRoute.to_json v)

let of_json j =
  Aws.Json.to_list VerifiedAccessInstanceOpenVpnClientConfigurationRoute.of_json j
