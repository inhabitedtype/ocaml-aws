type t = RouteTableAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map RouteTableAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list RouteTableAssociation.to_query v
let to_json v = `List (List.map RouteTableAssociation.to_json v)
let of_json j = Aws.Json.to_list RouteTableAssociation.of_json j
