open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; resource_id : String.t
  ; resource_cidr : String.t
  ; resource_region : String.t
  ; current_ipam_scope_id : String.t
  ; destination_ipam_scope_id : String.t option
  ; monitored : Boolean.t
  }

let make
    ?dry_run
    ~resource_id
    ~resource_cidr
    ~resource_region
    ~current_ipam_scope_id
    ?destination_ipam_scope_id
    ~monitored
    () =
  { dry_run
  ; resource_id
  ; resource_cidr
  ; resource_region
  ; current_ipam_scope_id
  ; destination_ipam_scope_id
  ; monitored
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; resource_id =
        Aws.Xml.required
          "ResourceId"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse)
    ; resource_cidr =
        Aws.Xml.required
          "ResourceCidr"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceCidr" xml) String.parse)
    ; resource_region =
        Aws.Xml.required
          "ResourceRegion"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceRegion" xml) String.parse)
    ; current_ipam_scope_id =
        Aws.Xml.required
          "CurrentIpamScopeId"
          (Aws.Util.option_bind (Aws.Xml.member "CurrentIpamScopeId" xml) String.parse)
    ; destination_ipam_scope_id =
        Aws.Util.option_bind (Aws.Xml.member "DestinationIpamScopeId" xml) String.parse
    ; monitored =
        Aws.Xml.required
          "Monitored"
          (Aws.Util.option_bind (Aws.Xml.member "Monitored" xml) Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Monitored", Boolean.to_query v.monitored))
       ; Aws.Util.option_map v.destination_ipam_scope_id (fun f ->
             Aws.Query.Pair ("DestinationIpamScopeId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("CurrentIpamScopeId", String.to_query v.current_ipam_scope_id))
       ; Some (Aws.Query.Pair ("ResourceRegion", String.to_query v.resource_region))
       ; Some (Aws.Query.Pair ("ResourceCidr", String.to_query v.resource_cidr))
       ; Some (Aws.Query.Pair ("ResourceId", String.to_query v.resource_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Monitored", Boolean.to_json v.monitored)
       ; Aws.Util.option_map v.destination_ipam_scope_id (fun f ->
             "DestinationIpamScopeId", String.to_json f)
       ; Some ("CurrentIpamScopeId", String.to_json v.current_ipam_scope_id)
       ; Some ("ResourceRegion", String.to_json v.resource_region)
       ; Some ("ResourceCidr", String.to_json v.resource_cidr)
       ; Some ("ResourceId", String.to_json v.resource_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; resource_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceId"))
  ; resource_cidr =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceCidr"))
  ; resource_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceRegion"))
  ; current_ipam_scope_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CurrentIpamScopeId"))
  ; destination_ipam_scope_id =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationIpamScopeId") String.of_json
  ; monitored = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Monitored"))
  }
