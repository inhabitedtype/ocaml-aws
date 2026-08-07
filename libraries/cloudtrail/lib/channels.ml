type t = Channel.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Channel.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list Channel.to_query v
let to_json v = `List (List.map Channel.to_json v)
let of_json j = Aws.Json.to_list Channel.of_json j
