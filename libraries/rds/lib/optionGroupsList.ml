type t = OptionGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map OptionGroup.parse (Aws.Xml.members "OptionGroup" xml))

let to_query v = Aws.Query.to_query_list OptionGroup.to_query v
let to_json v = `List (List.map OptionGroup.to_json v)
let of_json j = Aws.Json.to_list OptionGroup.of_json j
