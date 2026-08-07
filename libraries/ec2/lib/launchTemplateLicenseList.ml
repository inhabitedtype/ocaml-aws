type t = LaunchTemplateLicenseConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map LaunchTemplateLicenseConfiguration.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LaunchTemplateLicenseConfiguration.to_query v
let to_json v = `List (List.map LaunchTemplateLicenseConfiguration.to_json v)
let of_json j = Aws.Json.to_list LaunchTemplateLicenseConfiguration.of_json j
