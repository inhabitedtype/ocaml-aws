type t = VpcEncryptionControl.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VpcEncryptionControl.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpcEncryptionControl.to_query v
let to_json v = `List (List.map VpcEncryptionControl.to_json v)
let of_json j = Aws.Json.to_list VpcEncryptionControl.of_json j
