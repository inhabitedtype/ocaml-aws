open Aws.BaseTypes

type t = Blob.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Blob.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list Blob.to_query v
let to_json v = `List (List.map Blob.to_json v)
let of_json j = Aws.Json.to_list Blob.of_json j
