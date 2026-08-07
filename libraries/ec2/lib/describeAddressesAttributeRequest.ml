open Aws.BaseTypes

type t =
  { allocation_ids : AllocationIds.t
  ; attribute : AddressAttributeName.t option
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; dry_run : Boolean.t option
  }

let make ?(allocation_ids = []) ?attribute ?next_token ?max_results ?dry_run () =
  { allocation_ids; attribute; next_token; max_results; dry_run }

let parse xml =
  Some
    { allocation_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "AllocationId" xml) AllocationIds.parse)
    ; attribute =
        Aws.Util.option_bind (Aws.Xml.member "Attribute" xml) AddressAttributeName.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.attribute (fun f ->
             Aws.Query.Pair ("Attribute", AddressAttributeName.to_query f))
       ; Some (Aws.Query.Pair ("AllocationId", AllocationIds.to_query v.allocation_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.attribute (fun f ->
             "Attribute", AddressAttributeName.to_json f)
       ; Some ("AllocationId", AllocationIds.to_json v.allocation_ids)
       ])

let of_json j =
  { allocation_ids =
      AllocationIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AllocationId"))
  ; attribute =
      Aws.Util.option_map (Aws.Json.lookup j "Attribute") AddressAttributeName.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
