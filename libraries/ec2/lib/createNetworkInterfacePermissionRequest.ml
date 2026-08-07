open Aws.BaseTypes

type t =
  { network_interface_id : String.t
  ; aws_account_id : String.t option
  ; aws_service : String.t option
  ; permission : InterfacePermissionType.t
  ; dry_run : Boolean.t option
  }

let make ~network_interface_id ?aws_account_id ?aws_service ~permission ?dry_run () =
  { network_interface_id; aws_account_id; aws_service; permission; dry_run }

let parse xml =
  Some
    { network_interface_id =
        Aws.Xml.required
          "NetworkInterfaceId"
          (Aws.Util.option_bind (Aws.Xml.member "NetworkInterfaceId" xml) String.parse)
    ; aws_account_id =
        Aws.Util.option_bind (Aws.Xml.member "AwsAccountId" xml) String.parse
    ; aws_service = Aws.Util.option_bind (Aws.Xml.member "AwsService" xml) String.parse
    ; permission =
        Aws.Xml.required
          "Permission"
          (Aws.Util.option_bind
             (Aws.Xml.member "Permission" xml)
             InterfacePermissionType.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("Permission", InterfacePermissionType.to_query v.permission))
       ; Aws.Util.option_map v.aws_service (fun f ->
             Aws.Query.Pair ("AwsService", String.to_query f))
       ; Aws.Util.option_map v.aws_account_id (fun f ->
             Aws.Query.Pair ("AwsAccountId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("NetworkInterfaceId", String.to_query v.network_interface_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("Permission", InterfacePermissionType.to_json v.permission)
       ; Aws.Util.option_map v.aws_service (fun f -> "AwsService", String.to_json f)
       ; Aws.Util.option_map v.aws_account_id (fun f -> "AwsAccountId", String.to_json f)
       ; Some ("NetworkInterfaceId", String.to_json v.network_interface_id)
       ])

let of_json j =
  { network_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInterfaceId"))
  ; aws_account_id = Aws.Util.option_map (Aws.Json.lookup j "AwsAccountId") String.of_json
  ; aws_service = Aws.Util.option_map (Aws.Json.lookup j "AwsService") String.of_json
  ; permission =
      InterfacePermissionType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Permission"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
