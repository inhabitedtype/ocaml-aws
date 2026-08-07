type t = Dimension.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Dimension.parse (Aws.Xml.members "Dimension" xml))

let to_query v = Aws.Query.to_query_list Dimension.to_query v
let to_json v = `List (List.map Dimension.to_json v)
let of_json j = Aws.Json.to_list Dimension.of_json j
