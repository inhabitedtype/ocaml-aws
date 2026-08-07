type t = VerifiedAccessEndpointPortRange.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map VerifiedAccessEndpointPortRange.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VerifiedAccessEndpointPortRange.to_query v
let to_json v = `List (List.map VerifiedAccessEndpointPortRange.to_json v)
let of_json j = Aws.Json.to_list VerifiedAccessEndpointPortRange.of_json j
