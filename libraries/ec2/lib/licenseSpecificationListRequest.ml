type t = LicenseConfigurationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map LicenseConfigurationRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LicenseConfigurationRequest.to_query v
let to_json v = `List (List.map LicenseConfigurationRequest.to_json v)
let of_json j = Aws.Json.to_list LicenseConfigurationRequest.of_json j
