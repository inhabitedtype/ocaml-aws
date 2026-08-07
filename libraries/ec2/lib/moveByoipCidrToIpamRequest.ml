open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; cidr : String.t
  ; ipam_pool_id : String.t
  ; ipam_pool_owner : String.t
  }

let make ?dry_run ~cidr ~ipam_pool_id ~ipam_pool_owner () =
  { dry_run; cidr; ipam_pool_id; ipam_pool_owner }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; cidr =
        Aws.Xml.required
          "Cidr"
          (Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse)
    ; ipam_pool_id =
        Aws.Xml.required
          "IpamPoolId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPoolId" xml) String.parse)
    ; ipam_pool_owner =
        Aws.Xml.required
          "IpamPoolOwner"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPoolOwner" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("IpamPoolOwner", String.to_query v.ipam_pool_owner))
       ; Some (Aws.Query.Pair ("IpamPoolId", String.to_query v.ipam_pool_id))
       ; Some (Aws.Query.Pair ("Cidr", String.to_query v.cidr))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("IpamPoolOwner", String.to_json v.ipam_pool_owner)
       ; Some ("IpamPoolId", String.to_json v.ipam_pool_id)
       ; Some ("Cidr", String.to_json v.cidr)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; cidr = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Cidr"))
  ; ipam_pool_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPoolId"))
  ; ipam_pool_owner =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPoolOwner"))
  }
