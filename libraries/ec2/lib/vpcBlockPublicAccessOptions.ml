open Aws.BaseTypes

type t =
  { aws_account_id : String.t option
  ; aws_region : String.t option
  ; state : VpcBlockPublicAccessState.t option
  ; internet_gateway_block_mode : InternetGatewayBlockMode.t option
  ; reason : String.t option
  ; last_update_timestamp : DateTime.t option
  ; managed_by : ManagedBy.t option
  ; exclusions_allowed : VpcBlockPublicAccessExclusionsAllowed.t option
  }

let make
    ?aws_account_id
    ?aws_region
    ?state
    ?internet_gateway_block_mode
    ?reason
    ?last_update_timestamp
    ?managed_by
    ?exclusions_allowed
    () =
  { aws_account_id
  ; aws_region
  ; state
  ; internet_gateway_block_mode
  ; reason
  ; last_update_timestamp
  ; managed_by
  ; exclusions_allowed
  }

let parse xml =
  Some
    { aws_account_id =
        Aws.Util.option_bind (Aws.Xml.member "awsAccountId" xml) String.parse
    ; aws_region = Aws.Util.option_bind (Aws.Xml.member "awsRegion" xml) String.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) VpcBlockPublicAccessState.parse
    ; internet_gateway_block_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "internetGatewayBlockMode" xml)
          InternetGatewayBlockMode.parse
    ; reason = Aws.Util.option_bind (Aws.Xml.member "reason" xml) String.parse
    ; last_update_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "lastUpdateTimestamp" xml) DateTime.parse
    ; managed_by = Aws.Util.option_bind (Aws.Xml.member "managedBy" xml) ManagedBy.parse
    ; exclusions_allowed =
        Aws.Util.option_bind
          (Aws.Xml.member "exclusionsAllowed" xml)
          VpcBlockPublicAccessExclusionsAllowed.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.exclusions_allowed (fun f ->
             Aws.Query.Pair
               ("ExclusionsAllowed", VpcBlockPublicAccessExclusionsAllowed.to_query f))
       ; Aws.Util.option_map v.managed_by (fun f ->
             Aws.Query.Pair ("ManagedBy", ManagedBy.to_query f))
       ; Aws.Util.option_map v.last_update_timestamp (fun f ->
             Aws.Query.Pair ("LastUpdateTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.reason (fun f ->
             Aws.Query.Pair ("Reason", String.to_query f))
       ; Aws.Util.option_map v.internet_gateway_block_mode (fun f ->
             Aws.Query.Pair
               ("InternetGatewayBlockMode", InternetGatewayBlockMode.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", VpcBlockPublicAccessState.to_query f))
       ; Aws.Util.option_map v.aws_region (fun f ->
             Aws.Query.Pair ("AwsRegion", String.to_query f))
       ; Aws.Util.option_map v.aws_account_id (fun f ->
             Aws.Query.Pair ("AwsAccountId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.exclusions_allowed (fun f ->
             "exclusionsAllowed", VpcBlockPublicAccessExclusionsAllowed.to_json f)
       ; Aws.Util.option_map v.managed_by (fun f -> "managedBy", ManagedBy.to_json f)
       ; Aws.Util.option_map v.last_update_timestamp (fun f ->
             "lastUpdateTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.reason (fun f -> "reason", String.to_json f)
       ; Aws.Util.option_map v.internet_gateway_block_mode (fun f ->
             "internetGatewayBlockMode", InternetGatewayBlockMode.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", VpcBlockPublicAccessState.to_json f)
       ; Aws.Util.option_map v.aws_region (fun f -> "awsRegion", String.to_json f)
       ; Aws.Util.option_map v.aws_account_id (fun f -> "awsAccountId", String.to_json f)
       ])

let of_json j =
  { aws_account_id = Aws.Util.option_map (Aws.Json.lookup j "awsAccountId") String.of_json
  ; aws_region = Aws.Util.option_map (Aws.Json.lookup j "awsRegion") String.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") VpcBlockPublicAccessState.of_json
  ; internet_gateway_block_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "internetGatewayBlockMode")
        InternetGatewayBlockMode.of_json
  ; reason = Aws.Util.option_map (Aws.Json.lookup j "reason") String.of_json
  ; last_update_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "lastUpdateTimestamp") DateTime.of_json
  ; managed_by = Aws.Util.option_map (Aws.Json.lookup j "managedBy") ManagedBy.of_json
  ; exclusions_allowed =
      Aws.Util.option_map
        (Aws.Json.lookup j "exclusionsAllowed")
        VpcBlockPublicAccessExclusionsAllowed.of_json
  }
