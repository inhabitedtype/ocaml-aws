type t = IPRange.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IPRange.parse (Aws.Xml.members "IPRange" xml))

let to_query v = Aws.Query.to_query_list IPRange.to_query v
let to_json v = `List (List.map IPRange.to_json v)
let of_json j = Aws.Json.to_list IPRange.of_json j
