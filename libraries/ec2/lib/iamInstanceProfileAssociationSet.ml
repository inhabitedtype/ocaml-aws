type t = IamInstanceProfileAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IamInstanceProfileAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IamInstanceProfileAssociation.to_query v
let to_json v = `List (List.map IamInstanceProfileAssociation.to_json v)
let of_json j = Aws.Json.to_list IamInstanceProfileAssociation.of_json j
