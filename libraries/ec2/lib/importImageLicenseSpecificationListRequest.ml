type t = ImportImageLicenseConfigurationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ImportImageLicenseConfigurationRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImportImageLicenseConfigurationRequest.to_query v
let to_json v = `List (List.map ImportImageLicenseConfigurationRequest.to_json v)
let of_json j = Aws.Json.to_list ImportImageLicenseConfigurationRequest.of_json j
