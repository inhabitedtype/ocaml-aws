type t = TrunkInterfaceAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TrunkInterfaceAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TrunkInterfaceAssociation.to_query v
let to_json v = `List (List.map TrunkInterfaceAssociation.to_json v)
let of_json j = Aws.Json.to_list TrunkInterfaceAssociation.of_json j
