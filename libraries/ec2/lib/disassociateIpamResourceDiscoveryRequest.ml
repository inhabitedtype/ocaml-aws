open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_resource_discovery_association_id : String.t
  }

let make ?dry_run ~ipam_resource_discovery_association_id () =
  { dry_run; ipam_resource_discovery_association_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_resource_discovery_association_id =
        Aws.Xml.required
          "IpamResourceDiscoveryAssociationId"
          (Aws.Util.option_bind
             (Aws.Xml.member "IpamResourceDiscoveryAssociationId" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "IpamResourceDiscoveryAssociationId"
              , String.to_query v.ipam_resource_discovery_association_id ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "IpamResourceDiscoveryAssociationId"
           , String.to_json v.ipam_resource_discovery_association_id )
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_resource_discovery_association_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamResourceDiscoveryAssociationId"))
  }
