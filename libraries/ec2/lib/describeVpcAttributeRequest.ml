open Aws.BaseTypes

type t =
  { attribute : VpcAttributeName.t
  ; vpc_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~attribute ~vpc_id ?dry_run () = { attribute; vpc_id; dry_run }

let parse xml =
  Some
    { attribute =
        Aws.Xml.required
          "Attribute"
          (Aws.Util.option_bind (Aws.Xml.member "Attribute" xml) VpcAttributeName.parse)
    ; vpc_id =
        Aws.Xml.required
          "VpcId"
          (Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Some (Aws.Query.Pair ("Attribute", VpcAttributeName.to_query v.attribute))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("VpcId", String.to_json v.vpc_id)
       ; Some ("Attribute", VpcAttributeName.to_json v.attribute)
       ])

let of_json j =
  { attribute =
      VpcAttributeName.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Attribute"))
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
