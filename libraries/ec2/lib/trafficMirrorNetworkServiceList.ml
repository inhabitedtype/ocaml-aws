type t = TrafficMirrorNetworkService.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TrafficMirrorNetworkService.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TrafficMirrorNetworkService.to_query v
let to_json v = `List (List.map TrafficMirrorNetworkService.to_json v)
let of_json j = Aws.Json.to_list TrafficMirrorNetworkService.of_json j
