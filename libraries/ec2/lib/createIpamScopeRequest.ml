open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_id : String.t
  ; description : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; client_token : String.t option
  ; external_authority_configuration : ExternalAuthorityConfiguration.t option
  }

let make
    ?dry_run
    ~ipam_id
    ?description
    ?(tag_specifications = [])
    ?client_token
    ?external_authority_configuration
    () =
  { dry_run
  ; ipam_id
  ; description
  ; tag_specifications
  ; client_token
  ; external_authority_configuration
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_id =
        Aws.Xml.required
          "IpamId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamId" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; external_authority_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "ExternalAuthorityConfiguration" xml)
          ExternalAuthorityConfiguration.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.external_authority_configuration (fun f ->
             Aws.Query.Pair
               ( "ExternalAuthorityConfiguration"
               , ExternalAuthorityConfiguration.to_query f ))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("IpamId", String.to_query v.ipam_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.external_authority_configuration (fun f ->
             "ExternalAuthorityConfiguration", ExternalAuthorityConfiguration.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("IpamId", String.to_json v.ipam_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; external_authority_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "ExternalAuthorityConfiguration")
        ExternalAuthorityConfiguration.of_json
  }
