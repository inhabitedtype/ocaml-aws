type t = ModifyVerifiedAccessEndpointPortRange.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ModifyVerifiedAccessEndpointPortRange.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ModifyVerifiedAccessEndpointPortRange.to_query v
let to_json v = `List (List.map ModifyVerifiedAccessEndpointPortRange.to_json v)
let of_json j = Aws.Json.to_list ModifyVerifiedAccessEndpointPortRange.of_json j
