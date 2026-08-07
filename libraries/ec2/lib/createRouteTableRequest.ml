open Aws.BaseTypes

type t =
  { tag_specifications : TagSpecificationList.t
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; vpc_id : String.t
  }

let make ?(tag_specifications = []) ?client_token ?dry_run ~vpc_id () =
  { tag_specifications; client_token; dry_run; vpc_id }

let parse xml =
  Some
    { tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; vpc_id =
        Aws.Xml.required
          "vpcId"
          (Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("vpcId", String.to_json v.vpc_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ])

let of_json j =
  { tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcId"))
  }
