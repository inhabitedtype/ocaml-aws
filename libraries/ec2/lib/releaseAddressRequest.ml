open Aws.BaseTypes

type t =
  { allocation_id : String.t option
  ; public_ip : String.t option
  ; network_border_group : String.t option
  ; dry_run : Boolean.t option
  }

let make ?allocation_id ?public_ip ?network_border_group ?dry_run () =
  { allocation_id; public_ip; network_border_group; dry_run }

let parse xml =
  Some
    { allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "AllocationId" xml) String.parse
    ; public_ip = Aws.Util.option_bind (Aws.Xml.member "PublicIp" xml) String.parse
    ; network_border_group =
        Aws.Util.option_bind (Aws.Xml.member "NetworkBorderGroup" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.network_border_group (fun f ->
             Aws.Query.Pair ("NetworkBorderGroup", String.to_query f))
       ; Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ; Aws.Util.option_map v.allocation_id (fun f ->
             Aws.Query.Pair ("AllocationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.network_border_group (fun f ->
             "NetworkBorderGroup", String.to_json f)
       ; Aws.Util.option_map v.public_ip (fun f -> "PublicIp", String.to_json f)
       ; Aws.Util.option_map v.allocation_id (fun f -> "AllocationId", String.to_json f)
       ])

let of_json j =
  { allocation_id = Aws.Util.option_map (Aws.Json.lookup j "AllocationId") String.of_json
  ; public_ip = Aws.Util.option_map (Aws.Json.lookup j "PublicIp") String.of_json
  ; network_border_group =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkBorderGroup") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
