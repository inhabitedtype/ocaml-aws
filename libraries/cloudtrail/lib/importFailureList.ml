type t = ImportFailureListItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ImportFailureListItem.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ImportFailureListItem.to_query v
let to_json v = `List (List.map ImportFailureListItem.to_json v)
let of_json j = Aws.Json.to_list ImportFailureListItem.of_json j
