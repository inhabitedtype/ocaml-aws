open Aws.BaseTypes

type t =
  { dhcp_options_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~dhcp_options_id ?dry_run () = { dhcp_options_id; dry_run }

let parse xml =
  Some
    { dhcp_options_id =
        Aws.Xml.required
          "DhcpOptionsId"
          (Aws.Util.option_bind (Aws.Xml.member "DhcpOptionsId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("DhcpOptionsId", String.to_query v.dhcp_options_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("DhcpOptionsId", String.to_json v.dhcp_options_id)
       ])

let of_json j =
  { dhcp_options_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DhcpOptionsId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
