type t = Annotation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Annotation.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list Annotation.to_query v
let to_json v = `List (List.map Annotation.to_json v)
let of_json j = Aws.Json.to_list Annotation.of_json j
