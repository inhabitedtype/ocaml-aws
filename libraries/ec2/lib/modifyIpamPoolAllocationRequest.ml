open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_pool_allocation_id : String.t
  ; description : String.t option
  }

let make ?dry_run ~ipam_pool_allocation_id ?description () =
  { dry_run; ipam_pool_allocation_id; description }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_pool_allocation_id =
        Aws.Xml.required
          "IpamPoolAllocationId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPoolAllocationId" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("IpamPoolAllocationId", String.to_query v.ipam_pool_allocation_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("IpamPoolAllocationId", String.to_json v.ipam_pool_allocation_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_pool_allocation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPoolAllocationId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
