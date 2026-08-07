type t = StackSetAutoDeploymentTargetSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map StackSetAutoDeploymentTargetSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackSetAutoDeploymentTargetSummary.to_query v
let to_json v = `List (List.map StackSetAutoDeploymentTargetSummary.to_json v)
let of_json j = Aws.Json.to_list StackSetAutoDeploymentTargetSummary.of_json j
