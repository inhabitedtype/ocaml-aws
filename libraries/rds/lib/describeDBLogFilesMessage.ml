open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; filename_contains : String.t option
  ; file_last_written : Long.t option
  ; file_size : Long.t option
  ; filters : FilterList.t
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make
    ~d_b_instance_identifier
    ?filename_contains
    ?file_last_written
    ?file_size
    ?(filters = [])
    ?max_records
    ?marker
    () =
  { d_b_instance_identifier
  ; filename_contains
  ; file_last_written
  ; file_size
  ; filters
  ; max_records
  ; marker
  }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
    ; filename_contains =
        Aws.Util.option_bind (Aws.Xml.member "FilenameContains" xml) String.parse
    ; file_last_written =
        Aws.Util.option_bind (Aws.Xml.member "FileLastWritten" xml) Long.parse
    ; file_size = Aws.Util.option_bind (Aws.Xml.member "FileSize" xml) Long.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.file_size (fun f ->
             Aws.Query.Pair ("FileSize", Long.to_query f))
       ; Aws.Util.option_map v.file_last_written (fun f ->
             Aws.Query.Pair ("FileLastWritten", Long.to_query f))
       ; Aws.Util.option_map v.filename_contains (fun f ->
             Aws.Query.Pair ("FilenameContains", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.file_size (fun f -> "FileSize", Long.to_json f)
       ; Aws.Util.option_map v.file_last_written (fun f ->
             "FileLastWritten", Long.to_json f)
       ; Aws.Util.option_map v.filename_contains (fun f ->
             "FilenameContains", String.to_json f)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; filename_contains =
      Aws.Util.option_map (Aws.Json.lookup j "FilenameContains") String.of_json
  ; file_last_written =
      Aws.Util.option_map (Aws.Json.lookup j "FileLastWritten") Long.of_json
  ; file_size = Aws.Util.option_map (Aws.Json.lookup j "FileSize") Long.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
