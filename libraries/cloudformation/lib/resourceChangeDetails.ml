type t = ResourceChangeDetail.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ResourceChangeDetail.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ResourceChangeDetail.to_query v
let to_json v = `List (List.map ResourceChangeDetail.to_json v)
let of_json j = Aws.Json.to_list ResourceChangeDetail.of_json j
