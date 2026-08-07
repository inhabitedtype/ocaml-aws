open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; vpc_id : String.t
  ; tag_specifications : TagSpecificationList.t
  }

let make ?dry_run ~vpc_id ?(tag_specifications = []) () =
  { dry_run; vpc_id; tag_specifications }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; vpc_id =
        Aws.Xml.required
          "VpcId"
          (Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse)
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
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("VpcId", String.to_json v.vpc_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcId"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
