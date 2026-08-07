open Aws.BaseTypes

type t =
  { allocation_type : AllocationType.t option
  ; count : Integer.t option
  ; allocation_metadata : CapacityAllocationMetadataList.t
  }

let make ?allocation_type ?count ?(allocation_metadata = []) () =
  { allocation_type; count; allocation_metadata }

let parse xml =
  Some
    { allocation_type =
        Aws.Util.option_bind (Aws.Xml.member "allocationType" xml) AllocationType.parse
    ; count = Aws.Util.option_bind (Aws.Xml.member "count" xml) Integer.parse
    ; allocation_metadata =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "allocationMetadataList" xml)
             CapacityAllocationMetadataList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AllocationMetadataList"
              , CapacityAllocationMetadataList.to_query v.allocation_metadata ))
       ; Aws.Util.option_map v.count (fun f ->
             Aws.Query.Pair ("Count", Integer.to_query f))
       ; Aws.Util.option_map v.allocation_type (fun f ->
             Aws.Query.Pair ("AllocationType", AllocationType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "allocationMetadataList"
           , CapacityAllocationMetadataList.to_json v.allocation_metadata )
       ; Aws.Util.option_map v.count (fun f -> "count", Integer.to_json f)
       ; Aws.Util.option_map v.allocation_type (fun f ->
             "allocationType", AllocationType.to_json f)
       ])

let of_json j =
  { allocation_type =
      Aws.Util.option_map (Aws.Json.lookup j "allocationType") AllocationType.of_json
  ; count = Aws.Util.option_map (Aws.Json.lookup j "count") Integer.of_json
  ; allocation_metadata =
      CapacityAllocationMetadataList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "allocationMetadataList"))
  }
