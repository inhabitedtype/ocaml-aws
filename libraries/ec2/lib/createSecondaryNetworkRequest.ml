open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; dry_run : Boolean.t option
  ; ipv4_cidr_block : String.t
  ; network_type : SecondaryNetworkType.t
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?client_token
    ?dry_run
    ~ipv4_cidr_block
    ~network_type
    ?(tag_specifications = [])
    () =
  { client_token; dry_run; ipv4_cidr_block; network_type; tag_specifications }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipv4_cidr_block =
        Aws.Xml.required
          "Ipv4CidrBlock"
          (Aws.Util.option_bind (Aws.Xml.member "Ipv4CidrBlock" xml) String.parse)
    ; network_type =
        Aws.Xml.required
          "NetworkType"
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkType" xml)
             SecondaryNetworkType.parse)
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
           (Aws.Query.Pair ("NetworkType", SecondaryNetworkType.to_query v.network_type))
       ; Some (Aws.Query.Pair ("Ipv4CidrBlock", String.to_query v.ipv4_cidr_block))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("NetworkType", SecondaryNetworkType.to_json v.network_type)
       ; Some ("Ipv4CidrBlock", String.to_json v.ipv4_cidr_block)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipv4_cidr_block =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Ipv4CidrBlock"))
  ; network_type =
      SecondaryNetworkType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkType"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
