open Aws.BaseTypes

type t =
  { resource_record_sets : ResourceRecordSets.t
  ; is_truncated : Boolean.t
  ; next_record_name : String.t option
  ; next_record_type : RRType.t option
  ; next_record_identifier : String.t option
  ; max_items : String.t
  }

let make
    ~resource_record_sets
    ~is_truncated
    ?next_record_name
    ?next_record_type
    ?next_record_identifier
    ~max_items
    () =
  { resource_record_sets
  ; is_truncated
  ; next_record_name
  ; next_record_type
  ; next_record_identifier
  ; max_items
  }

let parse xml =
  Some
    { resource_record_sets =
        Aws.Xml.required
          "ResourceRecordSets"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceRecordSets" xml)
             ResourceRecordSets.parse)
    ; is_truncated =
        Aws.Xml.required
          "IsTruncated"
          (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
    ; next_record_name =
        Aws.Util.option_bind (Aws.Xml.member "NextRecordName" xml) String.parse
    ; next_record_type =
        Aws.Util.option_bind (Aws.Xml.member "NextRecordType" xml) RRType.parse
    ; next_record_identifier =
        Aws.Util.option_bind (Aws.Xml.member "NextRecordIdentifier" xml) String.parse
    ; max_items =
        Aws.Xml.required
          "MaxItems"
          (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
       ; Aws.Util.option_map v.next_record_identifier (fun f ->
             Aws.Query.Pair ("NextRecordIdentifier", String.to_query f))
       ; Aws.Util.option_map v.next_record_type (fun f ->
             Aws.Query.Pair ("NextRecordType", RRType.to_query f))
       ; Aws.Util.option_map v.next_record_name (fun f ->
             Aws.Query.Pair ("NextRecordName", String.to_query f))
       ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
       ; Some
           (Aws.Query.Pair
              ( "ResourceRecordSets.member"
              , ResourceRecordSets.to_query v.resource_record_sets ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MaxItems", String.to_json v.max_items)
       ; Aws.Util.option_map v.next_record_identifier (fun f ->
             "NextRecordIdentifier", String.to_json f)
       ; Aws.Util.option_map v.next_record_type (fun f ->
             "NextRecordType", RRType.to_json f)
       ; Aws.Util.option_map v.next_record_name (fun f ->
             "NextRecordName", String.to_json f)
       ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
       ; Some ("ResourceRecordSets", ResourceRecordSets.to_json v.resource_record_sets)
       ])

let of_json j =
  { resource_record_sets =
      ResourceRecordSets.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceRecordSets"))
  ; is_truncated =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
  ; next_record_name =
      Aws.Util.option_map (Aws.Json.lookup j "NextRecordName") String.of_json
  ; next_record_type =
      Aws.Util.option_map (Aws.Json.lookup j "NextRecordType") RRType.of_json
  ; next_record_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "NextRecordIdentifier") String.of_json
  ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
  }
