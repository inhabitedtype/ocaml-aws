open Aws.BaseTypes

type t =
  { resource_id : String.t option
  ; resource_type : IpamPoolSourceResourceType.t option
  ; resource_region : String.t option
  ; resource_owner : String.t option
  }

let make ?resource_id ?resource_type ?resource_region ?resource_owner () =
  { resource_id; resource_type; resource_region; resource_owner }

let parse xml =
  Some
    { resource_id = Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourceType" xml)
          IpamPoolSourceResourceType.parse
    ; resource_region =
        Aws.Util.option_bind (Aws.Xml.member "ResourceRegion" xml) String.parse
    ; resource_owner =
        Aws.Util.option_bind (Aws.Xml.member "ResourceOwner" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_owner (fun f ->
             Aws.Query.Pair ("ResourceOwner", String.to_query f))
       ; Aws.Util.option_map v.resource_region (fun f ->
             Aws.Query.Pair ("ResourceRegion", String.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", IpamPoolSourceResourceType.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_owner (fun f -> "ResourceOwner", String.to_json f)
       ; Aws.Util.option_map v.resource_region (fun f ->
             "ResourceRegion", String.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "ResourceType", IpamPoolSourceResourceType.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "ResourceId", String.to_json f)
       ])

let of_json j =
  { resource_id = Aws.Util.option_map (Aws.Json.lookup j "ResourceId") String.of_json
  ; resource_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResourceType")
        IpamPoolSourceResourceType.of_json
  ; resource_region =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceRegion") String.of_json
  ; resource_owner =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceOwner") String.of_json
  }
