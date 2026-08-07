open Aws.BaseTypes

type t =
  { destination_cidr : String.t option
  ; state : String.t option
  ; route_origin : String.t option
  ; prefix_list_id : String.t option
  ; attachment_id : String.t option
  ; resource_id : String.t option
  ; resource_type : String.t option
  }

let make
    ?destination_cidr
    ?state
    ?route_origin
    ?prefix_list_id
    ?attachment_id
    ?resource_id
    ?resource_type
    () =
  { destination_cidr
  ; state
  ; route_origin
  ; prefix_list_id
  ; attachment_id
  ; resource_id
  ; resource_type
  }

let parse xml =
  Some
    { destination_cidr =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidr" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) String.parse
    ; route_origin = Aws.Util.option_bind (Aws.Xml.member "routeOrigin" xml) String.parse
    ; prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "prefixListId" xml) String.parse
    ; attachment_id =
        Aws.Util.option_bind (Aws.Xml.member "attachmentId" xml) String.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind (Aws.Xml.member "resourceType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", String.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.attachment_id (fun f ->
             Aws.Query.Pair ("AttachmentId", String.to_query f))
       ; Aws.Util.option_map v.prefix_list_id (fun f ->
             Aws.Query.Pair ("PrefixListId", String.to_query f))
       ; Aws.Util.option_map v.route_origin (fun f ->
             Aws.Query.Pair ("RouteOrigin", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr (fun f ->
             Aws.Query.Pair ("DestinationCidr", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_type (fun f -> "resourceType", String.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.attachment_id (fun f -> "attachmentId", String.to_json f)
       ; Aws.Util.option_map v.prefix_list_id (fun f -> "prefixListId", String.to_json f)
       ; Aws.Util.option_map v.route_origin (fun f -> "routeOrigin", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr (fun f ->
             "destinationCidr", String.to_json f)
       ])

let of_json j =
  { destination_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidr") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") String.of_json
  ; route_origin = Aws.Util.option_map (Aws.Json.lookup j "routeOrigin") String.of_json
  ; prefix_list_id = Aws.Util.option_map (Aws.Json.lookup j "prefixListId") String.of_json
  ; attachment_id = Aws.Util.option_map (Aws.Json.lookup j "attachmentId") String.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  ; resource_type = Aws.Util.option_map (Aws.Json.lookup j "resourceType") String.of_json
  }
