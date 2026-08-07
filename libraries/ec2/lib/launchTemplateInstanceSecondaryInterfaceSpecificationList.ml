type t = LaunchTemplateInstanceSecondaryInterfaceSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       LaunchTemplateInstanceSecondaryInterfaceSpecification.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list LaunchTemplateInstanceSecondaryInterfaceSpecification.to_query v

let to_json v =
  `List (List.map LaunchTemplateInstanceSecondaryInterfaceSpecification.to_json v)

let of_json j =
  Aws.Json.to_list LaunchTemplateInstanceSecondaryInterfaceSpecification.of_json j
