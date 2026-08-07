type t = TypeConfigurationIdentifier.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TypeConfigurationIdentifier.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list TypeConfigurationIdentifier.to_query v
let to_json v = `List (List.map TypeConfigurationIdentifier.to_json v)
let of_json j = Aws.Json.to_list TypeConfigurationIdentifier.of_json j
