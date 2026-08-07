type t = ResourceDetail.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ResourceDetail.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ResourceDetail.to_query v
let to_json v = `List (List.map ResourceDetail.to_json v)
let of_json j = Aws.Json.to_list ResourceDetail.of_json j
