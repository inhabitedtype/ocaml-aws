open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; pool_id : String.t
  ; cidr : String.t
  }

let make ?dry_run ~pool_id ~cidr () = { dry_run; pool_id; cidr }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; pool_id =
        Aws.Xml.required
          "PoolId"
          (Aws.Util.option_bind (Aws.Xml.member "PoolId" xml) String.parse)
    ; cidr =
        Aws.Xml.required
          "Cidr"
          (Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Cidr", String.to_query v.cidr))
       ; Some (Aws.Query.Pair ("PoolId", String.to_query v.pool_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Cidr", String.to_json v.cidr)
       ; Some ("PoolId", String.to_json v.pool_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; pool_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PoolId"))
  ; cidr = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Cidr"))
  }
