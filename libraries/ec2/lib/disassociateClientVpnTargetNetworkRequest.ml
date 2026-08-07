open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t
  ; association_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~client_vpn_endpoint_id ~association_id ?dry_run () =
  { client_vpn_endpoint_id; association_id; dry_run }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Xml.required
          "ClientVpnEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "ClientVpnEndpointId" xml) String.parse)
    ; association_id =
        Aws.Xml.required
          "AssociationId"
          (Aws.Util.option_bind (Aws.Xml.member "AssociationId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("AssociationId", String.to_query v.association_id))
       ; Some
           (Aws.Query.Pair
              ("ClientVpnEndpointId", String.to_query v.client_vpn_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("AssociationId", String.to_json v.association_id)
       ; Some ("ClientVpnEndpointId", String.to_json v.client_vpn_endpoint_id)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientVpnEndpointId"))
  ; association_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AssociationId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
