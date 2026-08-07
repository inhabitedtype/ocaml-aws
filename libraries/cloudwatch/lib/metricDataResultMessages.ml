type t = MessageData.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MessageData.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list MessageData.to_query v
let to_json v = `List (List.map MessageData.to_json v)
let of_json j = Aws.Json.to_list MessageData.of_json j
