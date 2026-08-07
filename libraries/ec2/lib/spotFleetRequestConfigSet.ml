type t = SpotFleetRequestConfig.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SpotFleetRequestConfig.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SpotFleetRequestConfig.to_query v
let to_json v = `List (List.map SpotFleetRequestConfig.to_json v)
let of_json j = Aws.Json.to_list SpotFleetRequestConfig.of_json j
