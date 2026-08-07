open Aws.BaseTypes

type t = Integer.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Integer.parse (Aws.Xml.members "OccurenceDay" xml))

let to_query v = Aws.Query.to_query_list Integer.to_query v
let to_json v = `List (List.map Integer.to_json v)
let of_json j = Aws.Json.to_list Integer.of_json j
