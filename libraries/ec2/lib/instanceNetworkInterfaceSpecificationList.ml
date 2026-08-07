type t = InstanceNetworkInterfaceSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InstanceNetworkInterfaceSpecification.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceNetworkInterfaceSpecification.to_query v
let to_json v = `List (List.map InstanceNetworkInterfaceSpecification.to_json v)
let of_json j = Aws.Json.to_list InstanceNetworkInterfaceSpecification.of_json j
