type t = ThroughResourcesStatement.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ThroughResourcesStatement.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ThroughResourcesStatement.to_query v
let to_json v = `List (List.map ThroughResourcesStatement.to_json v)
let of_json j = Aws.Json.to_list ThroughResourcesStatement.of_json j
