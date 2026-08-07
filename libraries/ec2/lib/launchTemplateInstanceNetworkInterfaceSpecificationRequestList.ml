type t = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       LaunchTemplateInstanceNetworkInterfaceSpecificationRequest.parse
       (Aws.Xml.members "InstanceNetworkInterfaceSpecification" xml))

let to_query v =
  Aws.Query.to_query_list
    LaunchTemplateInstanceNetworkInterfaceSpecificationRequest.to_query
    v

let to_json v =
  `List (List.map LaunchTemplateInstanceNetworkInterfaceSpecificationRequest.to_json v)

let of_json j =
  Aws.Json.to_list LaunchTemplateInstanceNetworkInterfaceSpecificationRequest.of_json j
