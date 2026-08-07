open Aws.BaseTypes

type t =
  { network_interface_permission_id : String.t option
  ; network_interface_id : String.t option
  ; aws_account_id : String.t option
  ; aws_service : String.t option
  ; permission : InterfacePermissionType.t option
  ; permission_state : NetworkInterfacePermissionState.t option
  }

let make
    ?network_interface_permission_id
    ?network_interface_id
    ?aws_account_id
    ?aws_service
    ?permission
    ?permission_state
    () =
  { network_interface_permission_id
  ; network_interface_id
  ; aws_account_id
  ; aws_service
  ; permission
  ; permission_state
  }

let parse xml =
  Some
    { network_interface_permission_id =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInterfacePermissionId" xml)
          String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; aws_account_id =
        Aws.Util.option_bind (Aws.Xml.member "awsAccountId" xml) String.parse
    ; aws_service = Aws.Util.option_bind (Aws.Xml.member "awsService" xml) String.parse
    ; permission =
        Aws.Util.option_bind
          (Aws.Xml.member "permission" xml)
          InterfacePermissionType.parse
    ; permission_state =
        Aws.Util.option_bind
          (Aws.Xml.member "permissionState" xml)
          NetworkInterfacePermissionState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.permission_state (fun f ->
             Aws.Query.Pair ("PermissionState", NetworkInterfacePermissionState.to_query f))
       ; Aws.Util.option_map v.permission (fun f ->
             Aws.Query.Pair ("Permission", InterfacePermissionType.to_query f))
       ; Aws.Util.option_map v.aws_service (fun f ->
             Aws.Query.Pair ("AwsService", String.to_query f))
       ; Aws.Util.option_map v.aws_account_id (fun f ->
             Aws.Query.Pair ("AwsAccountId", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.network_interface_permission_id (fun f ->
             Aws.Query.Pair ("NetworkInterfacePermissionId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.permission_state (fun f ->
             "permissionState", NetworkInterfacePermissionState.to_json f)
       ; Aws.Util.option_map v.permission (fun f ->
             "permission", InterfacePermissionType.to_json f)
       ; Aws.Util.option_map v.aws_service (fun f -> "awsService", String.to_json f)
       ; Aws.Util.option_map v.aws_account_id (fun f -> "awsAccountId", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.network_interface_permission_id (fun f ->
             "networkInterfacePermissionId", String.to_json f)
       ])

let of_json j =
  { network_interface_permission_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInterfacePermissionId")
        String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; aws_account_id = Aws.Util.option_map (Aws.Json.lookup j "awsAccountId") String.of_json
  ; aws_service = Aws.Util.option_map (Aws.Json.lookup j "awsService") String.of_json
  ; permission =
      Aws.Util.option_map (Aws.Json.lookup j "permission") InterfacePermissionType.of_json
  ; permission_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "permissionState")
        NetworkInterfacePermissionState.of_json
  }
