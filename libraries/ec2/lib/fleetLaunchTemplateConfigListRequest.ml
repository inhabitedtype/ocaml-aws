type t = FleetLaunchTemplateConfigRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map FleetLaunchTemplateConfigRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list FleetLaunchTemplateConfigRequest.to_query v
let to_json v = `List (List.map FleetLaunchTemplateConfigRequest.to_json v)
let of_json j = Aws.Json.to_list FleetLaunchTemplateConfigRequest.of_json j
