type t = VpcEncryptionNonCompliantResource.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map VpcEncryptionNonCompliantResource.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpcEncryptionNonCompliantResource.to_query v
let to_json v = `List (List.map VpcEncryptionNonCompliantResource.to_json v)
let of_json j = Aws.Json.to_list VpcEncryptionNonCompliantResource.of_json j
