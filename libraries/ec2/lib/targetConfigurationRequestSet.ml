type t = TargetConfigurationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       TargetConfigurationRequest.parse
       (Aws.Xml.members "TargetConfigurationRequest" xml))

let to_query v = Aws.Query.to_query_list TargetConfigurationRequest.to_query v
let to_json v = `List (List.map TargetConfigurationRequest.to_json v)
let of_json j = Aws.Json.to_list TargetConfigurationRequest.of_json j
