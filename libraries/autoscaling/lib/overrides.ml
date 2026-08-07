type t = LaunchTemplateOverrides.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map LaunchTemplateOverrides.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list LaunchTemplateOverrides.to_query v
let to_json v = `List (List.map LaunchTemplateOverrides.to_json v)
let of_json j = Aws.Json.to_list LaunchTemplateOverrides.of_json j
