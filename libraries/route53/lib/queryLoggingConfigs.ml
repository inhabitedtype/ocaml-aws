type t = QueryLoggingConfig.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map QueryLoggingConfig.parse (Aws.Xml.members "QueryLoggingConfig" xml))

let to_query v = Aws.Query.to_query_list QueryLoggingConfig.to_query v
let to_json v = `List (List.map QueryLoggingConfig.to_json v)
let of_json j = Aws.Json.to_list QueryLoggingConfig.of_json j
