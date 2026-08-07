type t = OptionGroupOption.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map OptionGroupOption.parse (Aws.Xml.members "OptionGroupOption" xml))

let to_query v = Aws.Query.to_query_list OptionGroupOption.to_query v
let to_json v = `List (List.map OptionGroupOption.to_json v)
let of_json j = Aws.Json.to_list OptionGroupOption.of_json j
