type t = ServerlessV2PlatformVersionInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ServerlessV2PlatformVersionInfo.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ServerlessV2PlatformVersionInfo.to_query v
let to_json v = `List (List.map ServerlessV2PlatformVersionInfo.to_json v)
let of_json j = Aws.Json.to_list ServerlessV2PlatformVersionInfo.of_json j
