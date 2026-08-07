type t = ResourceTag.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ResourceTag.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ResourceTag.to_query v
let to_json v = `List (List.map ResourceTag.to_json v)
let of_json j = Aws.Json.to_list ResourceTag.of_json j
