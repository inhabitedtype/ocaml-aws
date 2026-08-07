open Aws.BaseTypes

type t =
  { vpn_concentrator_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~vpn_concentrator_id ?dry_run () = { vpn_concentrator_id; dry_run }

let parse xml =
  Some
    { vpn_concentrator_id =
        Aws.Xml.required
          "VpnConcentratorId"
          (Aws.Util.option_bind (Aws.Xml.member "VpnConcentratorId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("VpnConcentratorId", String.to_query v.vpn_concentrator_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("VpnConcentratorId", String.to_json v.vpn_concentrator_id)
       ])

let of_json j =
  { vpn_concentrator_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpnConcentratorId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
