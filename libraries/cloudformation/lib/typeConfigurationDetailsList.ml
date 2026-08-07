type t = TypeConfigurationDetails.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TypeConfigurationDetails.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list TypeConfigurationDetails.to_query v
let to_json v = `List (List.map TypeConfigurationDetails.to_json v)
let of_json j = Aws.Json.to_list TypeConfigurationDetails.of_json j
