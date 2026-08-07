open Aws.BaseTypes

type t =
  { allocation_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~allocation_id ?dry_run () = { allocation_id; dry_run }

let parse xml =
  Some
    { allocation_id =
        Aws.Xml.required
          "AllocationId"
          (Aws.Util.option_bind (Aws.Xml.member "AllocationId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("AllocationId", String.to_query v.allocation_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("AllocationId", String.to_json v.allocation_id)
       ])

let of_json j =
  { allocation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AllocationId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
