type t = LaunchTemplateVersion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LaunchTemplateVersion.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LaunchTemplateVersion.to_query v
let to_json v = `List (List.map LaunchTemplateVersion.to_json v)
let of_json j = Aws.Json.to_list LaunchTemplateVersion.of_json j
