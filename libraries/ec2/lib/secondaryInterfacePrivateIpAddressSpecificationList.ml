type t = SecondaryInterfacePrivateIpAddressSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       SecondaryInterfacePrivateIpAddressSpecification.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list SecondaryInterfacePrivateIpAddressSpecification.to_query v

let to_json v = `List (List.map SecondaryInterfacePrivateIpAddressSpecification.to_json v)
let of_json j = Aws.Json.to_list SecondaryInterfacePrivateIpAddressSpecification.of_json j
