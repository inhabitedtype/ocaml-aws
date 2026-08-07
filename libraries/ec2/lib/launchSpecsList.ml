type t = SpotFleetLaunchSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SpotFleetLaunchSpecification.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SpotFleetLaunchSpecification.to_query v
let to_json v = `List (List.map SpotFleetLaunchSpecification.to_json v)
let of_json j = Aws.Json.to_list SpotFleetLaunchSpecification.of_json j
