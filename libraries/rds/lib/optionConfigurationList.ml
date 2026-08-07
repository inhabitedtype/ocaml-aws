type t = OptionConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map OptionConfiguration.parse (Aws.Xml.members "OptionConfiguration" xml))

let to_query v = Aws.Query.to_query_list OptionConfiguration.to_query v
let to_json v = `List (List.map OptionConfiguration.to_json v)
let of_json j = Aws.Json.to_list OptionConfiguration.of_json j
