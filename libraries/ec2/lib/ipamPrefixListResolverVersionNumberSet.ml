open Aws.BaseTypes

type t = Long.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Long.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list Long.to_query v
let to_json v = `List (List.map Long.to_json v)
let of_json j = Aws.Json.to_list Long.of_json j
