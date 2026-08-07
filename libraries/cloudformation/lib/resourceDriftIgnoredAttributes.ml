type t = ResourceDriftIgnoredAttribute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ResourceDriftIgnoredAttribute.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ResourceDriftIgnoredAttribute.to_query v
let to_json v = `List (List.map ResourceDriftIgnoredAttribute.to_json v)
let of_json j = Aws.Json.to_list ResourceDriftIgnoredAttribute.of_json j
