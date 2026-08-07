type t = LaunchTemplateLicenseConfigurationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       LaunchTemplateLicenseConfigurationRequest.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list LaunchTemplateLicenseConfigurationRequest.to_query v

let to_json v = `List (List.map LaunchTemplateLicenseConfigurationRequest.to_json v)
let of_json j = Aws.Json.to_list LaunchTemplateLicenseConfigurationRequest.of_json j
