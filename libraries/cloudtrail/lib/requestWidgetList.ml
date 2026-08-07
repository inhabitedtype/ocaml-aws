type t = RequestWidget.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RequestWidget.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list RequestWidget.to_query v
let to_json v = `List (List.map RequestWidget.to_json v)
let of_json j = Aws.Json.to_list RequestWidget.of_json j
