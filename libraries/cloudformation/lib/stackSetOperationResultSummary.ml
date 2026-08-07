open Aws.BaseTypes

type t =
  { account : String.t option
  ; region : String.t option
  ; status : StackSetOperationResultStatus.t option
  ; status_reason : String.t option
  ; account_gate_result : AccountGateResult.t option
  ; organizational_unit_id : String.t option
  }

let make
    ?account
    ?region
    ?status
    ?status_reason
    ?account_gate_result
    ?organizational_unit_id
    () =
  { account; region; status; status_reason; account_gate_result; organizational_unit_id }

let parse xml =
  Some
    { account = Aws.Util.option_bind (Aws.Xml.member "Account" xml) String.parse
    ; region = Aws.Util.option_bind (Aws.Xml.member "Region" xml) String.parse
    ; status =
        Aws.Util.option_bind
          (Aws.Xml.member "Status" xml)
          StackSetOperationResultStatus.parse
    ; status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
    ; account_gate_result =
        Aws.Util.option_bind
          (Aws.Xml.member "AccountGateResult" xml)
          AccountGateResult.parse
    ; organizational_unit_id =
        Aws.Util.option_bind (Aws.Xml.member "OrganizationalUnitId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organizational_unit_id (fun f ->
             Aws.Query.Pair ("OrganizationalUnitId", String.to_query f))
       ; Aws.Util.option_map v.account_gate_result (fun f ->
             Aws.Query.Pair ("AccountGateResult", AccountGateResult.to_query f))
       ; Aws.Util.option_map v.status_reason (fun f ->
             Aws.Query.Pair ("StatusReason", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", StackSetOperationResultStatus.to_query f))
       ; Aws.Util.option_map v.region (fun f ->
             Aws.Query.Pair ("Region", String.to_query f))
       ; Aws.Util.option_map v.account (fun f ->
             Aws.Query.Pair ("Account", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organizational_unit_id (fun f ->
             "OrganizationalUnitId", String.to_json f)
       ; Aws.Util.option_map v.account_gate_result (fun f ->
             "AccountGateResult", AccountGateResult.to_json f)
       ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "Status", StackSetOperationResultStatus.to_json f)
       ; Aws.Util.option_map v.region (fun f -> "Region", String.to_json f)
       ; Aws.Util.option_map v.account (fun f -> "Account", String.to_json f)
       ])

let of_json j =
  { account = Aws.Util.option_map (Aws.Json.lookup j "Account") String.of_json
  ; region = Aws.Util.option_map (Aws.Json.lookup j "Region") String.of_json
  ; status =
      Aws.Util.option_map
        (Aws.Json.lookup j "Status")
        StackSetOperationResultStatus.of_json
  ; status_reason = Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
  ; account_gate_result =
      Aws.Util.option_map
        (Aws.Json.lookup j "AccountGateResult")
        AccountGateResult.of_json
  ; organizational_unit_id =
      Aws.Util.option_map (Aws.Json.lookup j "OrganizationalUnitId") String.of_json
  }
