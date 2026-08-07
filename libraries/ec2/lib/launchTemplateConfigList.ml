type t = LaunchTemplateConfig.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LaunchTemplateConfig.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LaunchTemplateConfig.to_query v
let to_json v = `List (List.map LaunchTemplateConfig.to_json v)
let of_json j = Aws.Json.to_list LaunchTemplateConfig.of_json j
