type t = VerifiedAccessInstanceLoggingConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       VerifiedAccessInstanceLoggingConfiguration.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list VerifiedAccessInstanceLoggingConfiguration.to_query v

let to_json v = `List (List.map VerifiedAccessInstanceLoggingConfiguration.to_json v)
let of_json j = Aws.Json.to_list VerifiedAccessInstanceLoggingConfiguration.of_json j
