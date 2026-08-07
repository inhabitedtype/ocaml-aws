type t = ResettableElementName.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ResettableElementName.parse (Aws.Xml.members "ResettableElementName" xml))

let to_query v = Aws.Query.to_query_list ResettableElementName.to_query v
let to_json v = `List (List.map ResettableElementName.to_json v)
let of_json j = Aws.Json.to_list ResettableElementName.of_json j
