open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; route_table_id : String.t
  }

let make ?dry_run ~route_table_id () = { dry_run; route_table_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; route_table_id =
        Aws.Xml.required
          "routeTableId"
          (Aws.Util.option_bind (Aws.Xml.member "routeTableId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("RouteTableId", String.to_query v.route_table_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("routeTableId", String.to_json v.route_table_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; route_table_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "routeTableId"))
  }
