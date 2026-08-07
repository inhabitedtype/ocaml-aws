open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_pool_id : String.t
  ; cidr : String.t option
  ; cidr_authorization_context : IpamCidrAuthorizationContext.t option
  ; netmask_length : Integer.t option
  ; client_token : String.t option
  ; verification_method : VerificationMethod.t option
  ; ipam_external_resource_verification_token_id : String.t option
  }

let make
    ?dry_run
    ~ipam_pool_id
    ?cidr
    ?cidr_authorization_context
    ?netmask_length
    ?client_token
    ?verification_method
    ?ipam_external_resource_verification_token_id
    () =
  { dry_run
  ; ipam_pool_id
  ; cidr
  ; cidr_authorization_context
  ; netmask_length
  ; client_token
  ; verification_method
  ; ipam_external_resource_verification_token_id
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_pool_id =
        Aws.Xml.required
          "IpamPoolId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPoolId" xml) String.parse)
    ; cidr = Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse
    ; cidr_authorization_context =
        Aws.Util.option_bind
          (Aws.Xml.member "CidrAuthorizationContext" xml)
          IpamCidrAuthorizationContext.parse
    ; netmask_length =
        Aws.Util.option_bind (Aws.Xml.member "NetmaskLength" xml) Integer.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; verification_method =
        Aws.Util.option_bind
          (Aws.Xml.member "VerificationMethod" xml)
          VerificationMethod.parse
    ; ipam_external_resource_verification_token_id =
        Aws.Util.option_bind
          (Aws.Xml.member "IpamExternalResourceVerificationTokenId" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_external_resource_verification_token_id (fun f ->
             Aws.Query.Pair ("IpamExternalResourceVerificationTokenId", String.to_query f))
       ; Aws.Util.option_map v.verification_method (fun f ->
             Aws.Query.Pair ("VerificationMethod", VerificationMethod.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.netmask_length (fun f ->
             Aws.Query.Pair ("NetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.cidr_authorization_context (fun f ->
             Aws.Query.Pair
               ("CidrAuthorizationContext", IpamCidrAuthorizationContext.to_query f))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ; Some (Aws.Query.Pair ("IpamPoolId", String.to_query v.ipam_pool_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_external_resource_verification_token_id (fun f ->
             "IpamExternalResourceVerificationTokenId", String.to_json f)
       ; Aws.Util.option_map v.verification_method (fun f ->
             "VerificationMethod", VerificationMethod.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.netmask_length (fun f ->
             "NetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.cidr_authorization_context (fun f ->
             "CidrAuthorizationContext", IpamCidrAuthorizationContext.to_json f)
       ; Aws.Util.option_map v.cidr (fun f -> "Cidr", String.to_json f)
       ; Some ("IpamPoolId", String.to_json v.ipam_pool_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_pool_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPoolId"))
  ; cidr = Aws.Util.option_map (Aws.Json.lookup j "Cidr") String.of_json
  ; cidr_authorization_context =
      Aws.Util.option_map
        (Aws.Json.lookup j "CidrAuthorizationContext")
        IpamCidrAuthorizationContext.of_json
  ; netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "NetmaskLength") Integer.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; verification_method =
      Aws.Util.option_map
        (Aws.Json.lookup j "VerificationMethod")
        VerificationMethod.of_json
  ; ipam_external_resource_verification_token_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "IpamExternalResourceVerificationTokenId")
        String.of_json
  }
