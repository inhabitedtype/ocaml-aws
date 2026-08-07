open Aws.BaseTypes

type t = DateTime.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DateTime.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list DateTime.to_query v
let to_json v = `List (List.map DateTime.to_json v)
let of_json j = Aws.Json.to_list DateTime.of_json j
