type t = VpcAttachment.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VpcAttachment.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpcAttachment.to_query v
let to_json v = `List (List.map VpcAttachment.to_json v)
let of_json j = Aws.Json.to_list VpcAttachment.of_json j
