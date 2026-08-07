type t = ParameterDeclaration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ParameterDeclaration.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ParameterDeclaration.to_query v
let to_json v = `List (List.map ParameterDeclaration.to_json v)
let of_json j = Aws.Json.to_list ParameterDeclaration.of_json j
