type t = Message.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Message.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list Message.to_query v
let to_json v = `List (List.map Message.to_json v)
let of_json j = Aws.Json.to_list Message.of_json j
