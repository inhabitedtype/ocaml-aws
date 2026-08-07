open Aws.BaseTypes

type t =
  { instance_ids : InstanceIdUpdateStringList.t
  ; sql_server_credentials : String.t option
  ; dry_run : Boolean.t option
  }

let make ~instance_ids ?sql_server_credentials ?dry_run () =
  { instance_ids; sql_server_credentials; dry_run }

let parse xml =
  Some
    { instance_ids =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceId" xml)
             InstanceIdUpdateStringList.parse)
    ; sql_server_credentials =
        Aws.Util.option_bind (Aws.Xml.member "SqlServerCredentials" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.sql_server_credentials (fun f ->
             Aws.Query.Pair ("SqlServerCredentials", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("InstanceId", InstanceIdUpdateStringList.to_query v.instance_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.sql_server_credentials (fun f ->
             "SqlServerCredentials", String.to_json f)
       ; Some ("InstanceId", InstanceIdUpdateStringList.to_json v.instance_ids)
       ])

let of_json j =
  { instance_ids =
      InstanceIdUpdateStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; sql_server_credentials =
      Aws.Util.option_map (Aws.Json.lookup j "SqlServerCredentials") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
