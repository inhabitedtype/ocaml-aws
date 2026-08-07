open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_pool_id : String.t
  ; cidr : String.t option
  }

let make ?dry_run ~ipam_pool_id ?cidr () = { dry_run; ipam_pool_id; cidr }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_pool_id =
        Aws.Xml.required
          "IpamPoolId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPoolId" xml) String.parse)
    ; cidr = Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ; Some (Aws.Query.Pair ("IpamPoolId", String.to_query v.ipam_pool_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr (fun f -> "Cidr", String.to_json f)
       ; Some ("IpamPoolId", String.to_json v.ipam_pool_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_pool_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPoolId"))
  ; cidr = Aws.Util.option_map (Aws.Json.lookup j "Cidr") String.of_json
  }
