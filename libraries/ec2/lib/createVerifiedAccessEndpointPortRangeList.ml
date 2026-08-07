type t = CreateVerifiedAccessEndpointPortRange.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CreateVerifiedAccessEndpointPortRange.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CreateVerifiedAccessEndpointPortRange.to_query v
let to_json v = `List (List.map CreateVerifiedAccessEndpointPortRange.to_json v)
let of_json j = Aws.Json.to_list CreateVerifiedAccessEndpointPortRange.of_json j
