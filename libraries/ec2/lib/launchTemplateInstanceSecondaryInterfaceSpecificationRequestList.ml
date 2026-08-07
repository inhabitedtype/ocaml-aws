type t = LaunchTemplateInstanceSecondaryInterfaceSpecificationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       LaunchTemplateInstanceSecondaryInterfaceSpecificationRequest.parse
       (Aws.Xml.members "InstanceSecondaryInterfaceSpecification" xml))

let to_query v =
  Aws.Query.to_query_list
    LaunchTemplateInstanceSecondaryInterfaceSpecificationRequest.to_query
    v

let to_json v =
  `List (List.map LaunchTemplateInstanceSecondaryInterfaceSpecificationRequest.to_json v)

let of_json j =
  Aws.Json.to_list LaunchTemplateInstanceSecondaryInterfaceSpecificationRequest.of_json j
