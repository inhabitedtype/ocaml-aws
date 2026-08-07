open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; network_interface_id : String.t
  ; source_dest_check : String.t option
  }

let make ?dry_run ~network_interface_id ?source_dest_check () =
  { dry_run; network_interface_id; source_dest_check }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; network_interface_id =
        Aws.Xml.required
          "networkInterfaceId"
          (Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse)
    ; source_dest_check =
        Aws.Util.option_bind (Aws.Xml.member "sourceDestCheck" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_dest_check (fun f ->
             Aws.Query.Pair ("SourceDestCheck", String.to_query f))
       ; Some
           (Aws.Query.Pair ("NetworkInterfaceId", String.to_query v.network_interface_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_dest_check (fun f ->
             "sourceDestCheck", String.to_json f)
       ; Some ("networkInterfaceId", String.to_json v.network_interface_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; network_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceId"))
  ; source_dest_check =
      Aws.Util.option_map (Aws.Json.lookup j "sourceDestCheck") String.of_json
  }
