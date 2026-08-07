open Aws.BaseTypes

type t =
  { type_ : NatGatewayApplianceType.t option
  ; appliance_arn : String.t option
  ; vpc_endpoint_id : String.t option
  ; attachment_state : NatGatewayApplianceState.t option
  ; modification_state : NatGatewayApplianceModifyState.t option
  ; failure_code : String.t option
  ; failure_message : String.t option
  }

let make
    ?type_
    ?appliance_arn
    ?vpc_endpoint_id
    ?attachment_state
    ?modification_state
    ?failure_code
    ?failure_message
    () =
  { type_
  ; appliance_arn
  ; vpc_endpoint_id
  ; attachment_state
  ; modification_state
  ; failure_code
  ; failure_message
  }

let parse xml =
  Some
    { type_ =
        Aws.Util.option_bind (Aws.Xml.member "type" xml) NatGatewayApplianceType.parse
    ; appliance_arn =
        Aws.Util.option_bind (Aws.Xml.member "applianceArn" xml) String.parse
    ; vpc_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpointId" xml) String.parse
    ; attachment_state =
        Aws.Util.option_bind
          (Aws.Xml.member "attachmentState" xml)
          NatGatewayApplianceState.parse
    ; modification_state =
        Aws.Util.option_bind
          (Aws.Xml.member "modificationState" xml)
          NatGatewayApplianceModifyState.parse
    ; failure_code = Aws.Util.option_bind (Aws.Xml.member "failureCode" xml) String.parse
    ; failure_message =
        Aws.Util.option_bind (Aws.Xml.member "failureMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failure_message (fun f ->
             Aws.Query.Pair ("FailureMessage", String.to_query f))
       ; Aws.Util.option_map v.failure_code (fun f ->
             Aws.Query.Pair ("FailureCode", String.to_query f))
       ; Aws.Util.option_map v.modification_state (fun f ->
             Aws.Query.Pair
               ("ModificationState", NatGatewayApplianceModifyState.to_query f))
       ; Aws.Util.option_map v.attachment_state (fun f ->
             Aws.Query.Pair ("AttachmentState", NatGatewayApplianceState.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             Aws.Query.Pair ("VpcEndpointId", String.to_query f))
       ; Aws.Util.option_map v.appliance_arn (fun f ->
             Aws.Query.Pair ("ApplianceArn", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", NatGatewayApplianceType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failure_message (fun f ->
             "failureMessage", String.to_json f)
       ; Aws.Util.option_map v.failure_code (fun f -> "failureCode", String.to_json f)
       ; Aws.Util.option_map v.modification_state (fun f ->
             "modificationState", NatGatewayApplianceModifyState.to_json f)
       ; Aws.Util.option_map v.attachment_state (fun f ->
             "attachmentState", NatGatewayApplianceState.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             "vpcEndpointId", String.to_json f)
       ; Aws.Util.option_map v.appliance_arn (fun f -> "applianceArn", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", NatGatewayApplianceType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "type") NatGatewayApplianceType.of_json
  ; appliance_arn = Aws.Util.option_map (Aws.Json.lookup j "applianceArn") String.of_json
  ; vpc_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpointId") String.of_json
  ; attachment_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "attachmentState")
        NatGatewayApplianceState.of_json
  ; modification_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "modificationState")
        NatGatewayApplianceModifyState.of_json
  ; failure_code = Aws.Util.option_map (Aws.Json.lookup j "failureCode") String.of_json
  ; failure_message =
      Aws.Util.option_map (Aws.Json.lookup j "failureMessage") String.of_json
  }
