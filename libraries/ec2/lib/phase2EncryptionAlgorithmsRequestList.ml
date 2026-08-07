type t = Phase2EncryptionAlgorithmsRequestListValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       Phase2EncryptionAlgorithmsRequestListValue.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list Phase2EncryptionAlgorithmsRequestListValue.to_query v

let to_json v = `List (List.map Phase2EncryptionAlgorithmsRequestListValue.to_json v)
let of_json j = Aws.Json.to_list Phase2EncryptionAlgorithmsRequestListValue.of_json j
