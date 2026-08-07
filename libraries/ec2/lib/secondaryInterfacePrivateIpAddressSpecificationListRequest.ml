type t = SecondaryInterfacePrivateIpAddressSpecificationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       SecondaryInterfacePrivateIpAddressSpecificationRequest.parse
       (Aws.Xml.members "SecondaryInterfacePrivateIpAddressSpecification" xml))

let to_query v =
  Aws.Query.to_query_list
    SecondaryInterfacePrivateIpAddressSpecificationRequest.to_query
    v

let to_json v =
  `List (List.map SecondaryInterfacePrivateIpAddressSpecificationRequest.to_json v)

let of_json j =
  Aws.Json.to_list SecondaryInterfacePrivateIpAddressSpecificationRequest.of_json j
