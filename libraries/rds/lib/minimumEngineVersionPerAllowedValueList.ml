type t = MinimumEngineVersionPerAllowedValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       MinimumEngineVersionPerAllowedValue.parse
       (Aws.Xml.members "MinimumEngineVersionPerAllowedValue" xml))

let to_query v = Aws.Query.to_query_list MinimumEngineVersionPerAllowedValue.to_query v
let to_json v = `List (List.map MinimumEngineVersionPerAllowedValue.to_json v)
let of_json j = Aws.Json.to_list MinimumEngineVersionPerAllowedValue.of_json j
