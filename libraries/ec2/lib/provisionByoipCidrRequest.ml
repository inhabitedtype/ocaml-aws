open Aws.BaseTypes

type t =
  { cidr : String.t
  ; cidr_authorization_context : CidrAuthorizationContext.t option
  ; publicly_advertisable : Boolean.t option
  ; description : String.t option
  ; dry_run : Boolean.t option
  ; pool_tag_specifications : TagSpecificationList.t
  ; multi_region : Boolean.t option
  ; network_border_group : String.t option
  }

let make
    ~cidr
    ?cidr_authorization_context
    ?publicly_advertisable
    ?description
    ?dry_run
    ?(pool_tag_specifications = [])
    ?multi_region
    ?network_border_group
    () =
  { cidr
  ; cidr_authorization_context
  ; publicly_advertisable
  ; description
  ; dry_run
  ; pool_tag_specifications
  ; multi_region
  ; network_border_group
  }

let parse xml =
  Some
    { cidr =
        Aws.Xml.required
          "Cidr"
          (Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse)
    ; cidr_authorization_context =
        Aws.Util.option_bind
          (Aws.Xml.member "CidrAuthorizationContext" xml)
          CidrAuthorizationContext.parse
    ; publicly_advertisable =
        Aws.Util.option_bind (Aws.Xml.member "PubliclyAdvertisable" xml) Boolean.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; pool_tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PoolTagSpecification" xml)
             TagSpecificationList.parse)
    ; multi_region = Aws.Util.option_bind (Aws.Xml.member "MultiRegion" xml) Boolean.parse
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "NetworkBorderGroup" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.multi_region (fun f ->
             Aws.Query.Pair ("MultiRegion", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PoolTagSpecification"
              , TagSpecificationList.to_query v.pool_tag_specifications ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.publicly_advertisable (fun f ->
             Aws.Query.Pair ("PubliclyAdvertisable", Boolean.to_query f))
       ; Aws.Util.option_map v.cidr_authorization_context (fun f ->
             Aws.Query.Pair
               ("CidrAuthorizationContext", CidrAuthorizationContext.to_query f))
       ; Some (Aws.Query.Pair ("Cidr", String.to_query v.cidr))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_border_group (fun f ->
             "NetworkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.multi_region (fun f -> "MultiRegion", Boolean.to_json f)
       ; Some
           ("PoolTagSpecification", TagSpecificationList.to_json v.pool_tag_specifications)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.publicly_advertisable (fun f ->
             "PubliclyAdvertisable", Boolean.to_json f)
       ; Aws.Util.option_map v.cidr_authorization_context (fun f ->
             "CidrAuthorizationContext", CidrAuthorizationContext.to_json f)
       ; Some ("Cidr", String.to_json v.cidr)
       ])

let of_json j =
  { cidr = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Cidr"))
  ; cidr_authorization_context =
      Aws.Util.option_map
        (Aws.Json.lookup j "CidrAuthorizationContext")
        CidrAuthorizationContext.of_json
  ; publicly_advertisable =
      Aws.Util.option_map (Aws.Json.lookup j "PubliclyAdvertisable") Boolean.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; pool_tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PoolTagSpecification"))
  ; multi_region = Aws.Util.option_map (Aws.Json.lookup j "MultiRegion") Boolean.of_json
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkBorderGroup") String.of_json
  }
