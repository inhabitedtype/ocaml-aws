open Aws.BaseTypes

type t =
  { subnet_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~subnet_id ?dry_run () = { subnet_id; dry_run }

let parse xml =
  Some
    { subnet_id =
        Aws.Xml.required
          "SubnetId"
          (Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("SubnetId", String.to_query v.subnet_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("SubnetId", String.to_json v.subnet_id)
       ])

let of_json j =
  { subnet_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
