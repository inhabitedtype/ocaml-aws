type t = LaunchTemplateInstanceNetworkInterfaceSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       LaunchTemplateInstanceNetworkInterfaceSpecification.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list LaunchTemplateInstanceNetworkInterfaceSpecification.to_query v

let to_json v =
  `List (List.map LaunchTemplateInstanceNetworkInterfaceSpecification.to_json v)

let of_json j =
  Aws.Json.to_list LaunchTemplateInstanceNetworkInterfaceSpecification.of_json j
