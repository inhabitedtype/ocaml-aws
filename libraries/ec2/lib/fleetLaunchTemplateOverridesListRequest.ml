type t = FleetLaunchTemplateOverridesRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map FleetLaunchTemplateOverridesRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list FleetLaunchTemplateOverridesRequest.to_query v
let to_json v = `List (List.map FleetLaunchTemplateOverridesRequest.to_json v)
let of_json j = Aws.Json.to_list FleetLaunchTemplateOverridesRequest.of_json j
