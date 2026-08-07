open Aws.BaseTypes

type t =
  { route_server_id : String.t
  ; vpc_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~route_server_id ~vpc_id ?dry_run () = { route_server_id; vpc_id; dry_run }

let parse xml =
  Some
    { route_server_id =
        Aws.Xml.required
          "RouteServerId"
          (Aws.Util.option_bind (Aws.Xml.member "RouteServerId" xml) String.parse)
    ; vpc_id =
        Aws.Xml.required
          "VpcId"
          (Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Some (Aws.Query.Pair ("RouteServerId", String.to_query v.route_server_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("VpcId", String.to_json v.vpc_id)
       ; Some ("RouteServerId", String.to_json v.route_server_id)
       ])

let of_json j =
  { route_server_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RouteServerId"))
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
