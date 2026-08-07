type t = FleetLaunchTemplateOverrides.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map FleetLaunchTemplateOverrides.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list FleetLaunchTemplateOverrides.to_query v
let to_json v = `List (List.map FleetLaunchTemplateOverrides.to_json v)
let of_json j = Aws.Json.to_list FleetLaunchTemplateOverrides.of_json j
