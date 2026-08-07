type t = VpcCidrBlockAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map VpcCidrBlockAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpcCidrBlockAssociation.to_query v
let to_json v = `List (List.map VpcCidrBlockAssociation.to_json v)
let of_json j = Aws.Json.to_list VpcCidrBlockAssociation.of_json j
