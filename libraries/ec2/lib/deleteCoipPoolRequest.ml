open Aws.BaseTypes

type t =
  { coip_pool_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~coip_pool_id ?dry_run () = { coip_pool_id; dry_run }

let parse xml =
  Some
    { coip_pool_id =
        Aws.Xml.required
          "CoipPoolId"
          (Aws.Util.option_bind (Aws.Xml.member "CoipPoolId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("CoipPoolId", String.to_query v.coip_pool_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("CoipPoolId", String.to_json v.coip_pool_id)
       ])

let of_json j =
  { coip_pool_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CoipPoolId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
