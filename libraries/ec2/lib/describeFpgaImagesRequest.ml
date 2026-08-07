open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; fpga_image_ids : FpgaImageIdList.t
  ; owners : OwnerStringList.t
  ; filters : FilterList.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make
    ?dry_run
    ?(fpga_image_ids = [])
    ?(owners = [])
    ?(filters = [])
    ?next_token
    ?max_results
    () =
  { dry_run; fpga_image_ids; owners; filters; next_token; max_results }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; fpga_image_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "FpgaImageId" xml) FpgaImageIdList.parse)
    ; owners =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) OwnerStringList.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some (Aws.Query.Pair ("Owner", OwnerStringList.to_query v.owners))
       ; Some (Aws.Query.Pair ("FpgaImageId", FpgaImageIdList.to_query v.fpga_image_ids))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Some ("Owner", OwnerStringList.to_json v.owners)
       ; Some ("FpgaImageId", FpgaImageIdList.to_json v.fpga_image_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; fpga_image_ids =
      FpgaImageIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FpgaImageId"))
  ; owners = OwnerStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Owner"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
