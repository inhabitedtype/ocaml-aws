type t = VpcClassicLink.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VpcClassicLink.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpcClassicLink.to_query v
let to_json v = `List (List.map VpcClassicLink.to_json v)
let of_json j = Aws.Json.to_list VpcClassicLink.of_json j
