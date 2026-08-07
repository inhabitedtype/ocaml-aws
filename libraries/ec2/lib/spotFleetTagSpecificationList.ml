type t = SpotFleetTagSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SpotFleetTagSpecification.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SpotFleetTagSpecification.to_query v
let to_json v = `List (List.map SpotFleetTagSpecification.to_json v)
let of_json j = Aws.Json.to_list SpotFleetTagSpecification.of_json j
