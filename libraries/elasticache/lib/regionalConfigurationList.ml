type t = RegionalConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map RegionalConfiguration.parse (Aws.Xml.members "RegionalConfiguration" xml))

let to_query v = Aws.Query.to_query_list RegionalConfiguration.to_query v
let to_json v = `List (List.map RegionalConfiguration.to_json v)
let of_json j = Aws.Json.to_list RegionalConfiguration.of_json j
