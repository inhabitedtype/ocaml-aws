open Aws.BaseTypes

type t = Double.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Double.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list Double.to_query v
let to_json v = `List (List.map Double.to_json v)
let of_json j = Aws.Json.to_list Double.of_json j
