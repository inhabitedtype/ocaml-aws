open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_pool_id : String.t
  ; cidr : String.t
  ; ipam_pool_allocation_id : String.t
  }

let make ?dry_run ~ipam_pool_id ~cidr ~ipam_pool_allocation_id () =
  { dry_run; ipam_pool_id; cidr; ipam_pool_allocation_id }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_pool_id =
        Aws.Xml.required
          "IpamPoolId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPoolId" xml) String.parse)
    ; cidr =
        Aws.Xml.required
          "Cidr"
          (Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse)
    ; ipam_pool_allocation_id =
        Aws.Xml.required
          "IpamPoolAllocationId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPoolAllocationId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("IpamPoolAllocationId", String.to_query v.ipam_pool_allocation_id))
       ; Some (Aws.Query.Pair ("Cidr", String.to_query v.cidr))
       ; Some (Aws.Query.Pair ("IpamPoolId", String.to_query v.ipam_pool_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("IpamPoolAllocationId", String.to_json v.ipam_pool_allocation_id)
       ; Some ("Cidr", String.to_json v.cidr)
       ; Some ("IpamPoolId", String.to_json v.ipam_pool_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_pool_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPoolId"))
  ; cidr = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Cidr"))
  ; ipam_pool_allocation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPoolAllocationId"))
  }
