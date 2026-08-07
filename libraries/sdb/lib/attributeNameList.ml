open Aws.BaseTypes

type t = String.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map String.parse (Aws.Xml.members "AttributeName" xml))

let to_query v = Aws.Query.to_query_list String.to_query v
let to_json v = `List (List.map String.to_json v)
let of_json j = Aws.Json.to_list String.of_json j
