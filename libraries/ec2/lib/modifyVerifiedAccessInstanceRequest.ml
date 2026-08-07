open Aws.BaseTypes

type t =
  { verified_access_instance_id : String.t
  ; description : String.t option
  ; dry_run : Boolean.t option
  ; client_token : String.t option
  ; cidr_endpoints_custom_sub_domain : String.t option
  }

let make
    ~verified_access_instance_id
    ?description
    ?dry_run
    ?client_token
    ?cidr_endpoints_custom_sub_domain
    () =
  { verified_access_instance_id
  ; description
  ; dry_run
  ; client_token
  ; cidr_endpoints_custom_sub_domain
  }

let parse xml =
  Some
    { verified_access_instance_id =
        Aws.Xml.required
          "VerifiedAccessInstanceId"
          (Aws.Util.option_bind
             (Aws.Xml.member "VerifiedAccessInstanceId" xml)
             String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; cidr_endpoints_custom_sub_domain =
        Aws.Util.option_bind
          (Aws.Xml.member "CidrEndpointsCustomSubDomain" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_endpoints_custom_sub_domain (fun f ->
             Aws.Query.Pair ("CidrEndpointsCustomSubDomain", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VerifiedAccessInstanceId", String.to_query v.verified_access_instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_endpoints_custom_sub_domain (fun f ->
             "CidrEndpointsCustomSubDomain", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("VerifiedAccessInstanceId", String.to_json v.verified_access_instance_id)
       ])

let of_json j =
  { verified_access_instance_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VerifiedAccessInstanceId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; cidr_endpoints_custom_sub_domain =
      Aws.Util.option_map
        (Aws.Json.lookup j "CidrEndpointsCustomSubDomain")
        String.of_json
  }
