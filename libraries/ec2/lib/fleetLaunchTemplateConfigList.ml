type t = FleetLaunchTemplateConfig.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map FleetLaunchTemplateConfig.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list FleetLaunchTemplateConfig.to_query v
let to_json v = `List (List.map FleetLaunchTemplateConfig.to_json v)
let of_json j = Aws.Json.to_list FleetLaunchTemplateConfig.of_json j
