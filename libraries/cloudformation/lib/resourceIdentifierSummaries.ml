type t = ResourceIdentifierSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ResourceIdentifierSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ResourceIdentifierSummary.to_query v
let to_json v = `List (List.map ResourceIdentifierSummary.to_json v)
let of_json j = Aws.Json.to_list ResourceIdentifierSummary.of_json j
