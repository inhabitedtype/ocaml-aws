type t = UserGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map UserGroup.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list UserGroup.to_query v
let to_json v = `List (List.map UserGroup.to_json v)
let of_json j = Aws.Json.to_list UserGroup.of_json j
