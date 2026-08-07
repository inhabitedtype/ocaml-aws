type t = ResourceTagSet.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ResourceTagSet.parse (Aws.Xml.members "ResourceTagSet" xml))

let to_query v = Aws.Query.to_query_list ResourceTagSet.to_query v
let to_json v = `List (List.map ResourceTagSet.to_json v)
let of_json j = Aws.Json.to_list ResourceTagSet.of_json j
