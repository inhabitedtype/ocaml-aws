type t = InstanceIpv6AddressRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       InstanceIpv6AddressRequest.parse
       (Aws.Xml.members "InstanceIpv6Address" xml))

let to_query v = Aws.Query.to_query_list InstanceIpv6AddressRequest.to_query v
let to_json v = `List (List.map InstanceIpv6AddressRequest.to_json v)
let of_json j = Aws.Json.to_list InstanceIpv6AddressRequest.of_json j
