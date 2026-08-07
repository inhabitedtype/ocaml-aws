open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; subnet_id : String.t option
  ; vpc_id : String.t option
  ; internet_gateway_exclusion_mode : InternetGatewayExclusionMode.t
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?dry_run
    ?subnet_id
    ?vpc_id
    ~internet_gateway_exclusion_mode
    ?(tag_specifications = [])
    () =
  { dry_run; subnet_id; vpc_id; internet_gateway_exclusion_mode; tag_specifications }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; internet_gateway_exclusion_mode =
        Aws.Xml.required
          "InternetGatewayExclusionMode"
          (Aws.Util.option_bind
             (Aws.Xml.member "InternetGatewayExclusionMode" xml)
             InternetGatewayExclusionMode.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some
           (Aws.Query.Pair
              ( "InternetGatewayExclusionMode"
              , InternetGatewayExclusionMode.to_query v.internet_gateway_exclusion_mode ))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some
           ( "InternetGatewayExclusionMode"
           , InternetGatewayExclusionMode.to_json v.internet_gateway_exclusion_mode )
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "SubnetId", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "SubnetId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; internet_gateway_exclusion_mode =
      InternetGatewayExclusionMode.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InternetGatewayExclusionMode"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
