open Aws.BaseTypes

type t =
  { description : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; f_i_p_s_enabled : Boolean.t option
  ; cidr_endpoints_custom_sub_domain : String.t option
  }

let make
    ?description
    ?(tag_specifications = [])
    ?client_token
    ?dry_run
    ?f_i_p_s_enabled
    ?cidr_endpoints_custom_sub_domain
    () =
  { description
  ; tag_specifications
  ; client_token
  ; dry_run
  ; f_i_p_s_enabled
  ; cidr_endpoints_custom_sub_domain
  }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; f_i_p_s_enabled =
        Aws.Util.option_bind (Aws.Xml.member "FIPSEnabled" xml) Boolean.parse
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
       ; Aws.Util.option_map v.f_i_p_s_enabled (fun f ->
             Aws.Query.Pair ("FIPSEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_endpoints_custom_sub_domain (fun f ->
             "CidrEndpointsCustomSubDomain", String.to_json f)
       ; Aws.Util.option_map v.f_i_p_s_enabled (fun f -> "FIPSEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; f_i_p_s_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "FIPSEnabled") Boolean.of_json
  ; cidr_endpoints_custom_sub_domain =
      Aws.Util.option_map
        (Aws.Json.lookup j "CidrEndpointsCustomSubDomain")
        String.of_json
  }
