type t = VPC.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map VPC.parse (Aws.Xml.members "VPC" xml))
let to_query v = Aws.Query.to_query_list VPC.to_query v
let to_json v = `List (List.map VPC.to_json v)
let of_json j = Aws.Json.to_list VPC.of_json j
