open Aws.BaseTypes

type t =
  { ipam_external_resource_verification_token_id : String.t option
  ; ipam_external_resource_verification_token_arn : String.t option
  ; ipam_id : String.t option
  ; ipam_arn : String.t option
  ; ipam_region : String.t option
  ; token_value : String.t option
  ; token_name : String.t option
  ; not_after : DateTime.t option
  ; status : TokenState.t option
  ; tags : TagList.t
  ; state : IpamExternalResourceVerificationTokenState.t option
  }

let make
    ?ipam_external_resource_verification_token_id
    ?ipam_external_resource_verification_token_arn
    ?ipam_id
    ?ipam_arn
    ?ipam_region
    ?token_value
    ?token_name
    ?not_after
    ?status
    ?(tags = [])
    ?state
    () =
  { ipam_external_resource_verification_token_id
  ; ipam_external_resource_verification_token_arn
  ; ipam_id
  ; ipam_arn
  ; ipam_region
  ; token_value
  ; token_name
  ; not_after
  ; status
  ; tags
  ; state
  }

let parse xml =
  Some
    { ipam_external_resource_verification_token_id =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamExternalResourceVerificationTokenId" xml)
          String.parse
    ; ipam_external_resource_verification_token_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamExternalResourceVerificationTokenArn" xml)
          String.parse
    ; ipam_id = Aws.Util.option_bind (Aws.Xml.member "ipamId" xml) String.parse
    ; ipam_arn = Aws.Util.option_bind (Aws.Xml.member "ipamArn" xml) String.parse
    ; ipam_region = Aws.Util.option_bind (Aws.Xml.member "ipamRegion" xml) String.parse
    ; token_value = Aws.Util.option_bind (Aws.Xml.member "tokenValue" xml) String.parse
    ; token_name = Aws.Util.option_bind (Aws.Xml.member "tokenName" xml) String.parse
    ; not_after = Aws.Util.option_bind (Aws.Xml.member "notAfter" xml) DateTime.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) TokenState.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          IpamExternalResourceVerificationTokenState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair
               ("State", IpamExternalResourceVerificationTokenState.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", TokenState.to_query f))
       ; Aws.Util.option_map v.not_after (fun f ->
             Aws.Query.Pair ("NotAfter", DateTime.to_query f))
       ; Aws.Util.option_map v.token_name (fun f ->
             Aws.Query.Pair ("TokenName", String.to_query f))
       ; Aws.Util.option_map v.token_value (fun f ->
             Aws.Query.Pair ("TokenValue", String.to_query f))
       ; Aws.Util.option_map v.ipam_region (fun f ->
             Aws.Query.Pair ("IpamRegion", String.to_query f))
       ; Aws.Util.option_map v.ipam_arn (fun f ->
             Aws.Query.Pair ("IpamArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_id (fun f ->
             Aws.Query.Pair ("IpamId", String.to_query f))
       ; Aws.Util.option_map v.ipam_external_resource_verification_token_arn (fun f ->
             Aws.Query.Pair ("IpamExternalResourceVerificationTokenArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_external_resource_verification_token_id (fun f ->
             Aws.Query.Pair ("IpamExternalResourceVerificationTokenId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             "state", IpamExternalResourceVerificationTokenState.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.status (fun f -> "status", TokenState.to_json f)
       ; Aws.Util.option_map v.not_after (fun f -> "notAfter", DateTime.to_json f)
       ; Aws.Util.option_map v.token_name (fun f -> "tokenName", String.to_json f)
       ; Aws.Util.option_map v.token_value (fun f -> "tokenValue", String.to_json f)
       ; Aws.Util.option_map v.ipam_region (fun f -> "ipamRegion", String.to_json f)
       ; Aws.Util.option_map v.ipam_arn (fun f -> "ipamArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_id (fun f -> "ipamId", String.to_json f)
       ; Aws.Util.option_map v.ipam_external_resource_verification_token_arn (fun f ->
             "ipamExternalResourceVerificationTokenArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_external_resource_verification_token_id (fun f ->
             "ipamExternalResourceVerificationTokenId", String.to_json f)
       ])

let of_json j =
  { ipam_external_resource_verification_token_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamExternalResourceVerificationTokenId")
        String.of_json
  ; ipam_external_resource_verification_token_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamExternalResourceVerificationTokenArn")
        String.of_json
  ; ipam_id = Aws.Util.option_map (Aws.Json.lookup j "ipamId") String.of_json
  ; ipam_arn = Aws.Util.option_map (Aws.Json.lookup j "ipamArn") String.of_json
  ; ipam_region = Aws.Util.option_map (Aws.Json.lookup j "ipamRegion") String.of_json
  ; token_value = Aws.Util.option_map (Aws.Json.lookup j "tokenValue") String.of_json
  ; token_name = Aws.Util.option_map (Aws.Json.lookup j "tokenName") String.of_json
  ; not_after = Aws.Util.option_map (Aws.Json.lookup j "notAfter") DateTime.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") TokenState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        IpamExternalResourceVerificationTokenState.of_json
  }
