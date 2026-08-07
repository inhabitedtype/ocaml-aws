open Aws.BaseTypes

type t =
  { public_ips : PublicIpStringList.t
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  ; allocation_ids : AllocationIdList.t
  }

let make ?(public_ips = []) ?dry_run ?(filters = []) ?(allocation_ids = []) () =
  { public_ips; dry_run; filters; allocation_ids }

let parse xml =
  Some
    { public_ips =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "PublicIp" xml) PublicIpStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; allocation_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AllocationId" xml)
             AllocationIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("AllocationId", AllocationIdList.to_query v.allocation_ids))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("PublicIp", PublicIpStringList.to_query v.public_ips))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AllocationId", AllocationIdList.to_json v.allocation_ids)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("PublicIp", PublicIpStringList.to_json v.public_ips)
       ])

let of_json j =
  { public_ips =
      PublicIpStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PublicIp"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; allocation_ids =
      AllocationIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AllocationId"))
  }
