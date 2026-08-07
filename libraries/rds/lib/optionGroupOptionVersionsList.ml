type t = OptionVersion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map OptionVersion.parse (Aws.Xml.members "OptionVersion" xml))

let to_query v = Aws.Query.to_query_list OptionVersion.to_query v
let to_json v = `List (List.map OptionVersion.to_json v)
let of_json j = Aws.Json.to_list OptionVersion.of_json j
