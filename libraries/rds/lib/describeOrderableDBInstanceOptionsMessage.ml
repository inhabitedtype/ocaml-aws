open Aws.BaseTypes

type t =
  { engine : String.t
  ; engine_version : String.t option
  ; d_b_instance_class : String.t option
  ; license_model : String.t option
  ; availability_zone_group : String.t option
  ; vpc : Boolean.t option
  ; filters : FilterList.t
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make
    ~engine
    ?engine_version
    ?d_b_instance_class
    ?license_model
    ?availability_zone_group
    ?vpc
    ?(filters = [])
    ?max_records
    ?marker
    () =
  { engine
  ; engine_version
  ; d_b_instance_class
  ; license_model
  ; availability_zone_group
  ; vpc
  ; filters
  ; max_records
  ; marker
  }

let parse xml =
  Some
    { engine =
        Aws.Xml.required
          "Engine"
          (Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse)
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; d_b_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceClass" xml) String.parse
    ; license_model =
        Aws.Util.option_bind (Aws.Xml.member "LicenseModel" xml) String.parse
    ; availability_zone_group =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneGroup" xml) String.parse
    ; vpc = Aws.Util.option_bind (Aws.Xml.member "Vpc" xml) Boolean.parse
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
       ; Aws.Util.option_map v.vpc (fun f -> Aws.Query.Pair ("Vpc", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone_group (fun f ->
             Aws.Query.Pair ("AvailabilityZoneGroup", String.to_query f))
       ; Aws.Util.option_map v.license_model (fun f ->
             Aws.Query.Pair ("LicenseModel", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             Aws.Query.Pair ("DBInstanceClass", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Some (Aws.Query.Pair ("Engine", String.to_query v.engine))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.vpc (fun f -> "Vpc", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone_group (fun f ->
             "AvailabilityZoneGroup", String.to_json f)
       ; Aws.Util.option_map v.license_model (fun f -> "LicenseModel", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             "DBInstanceClass", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Some ("Engine", String.to_json v.engine)
       ])

let of_json j =
  { engine = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Engine"))
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; d_b_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceClass") String.of_json
  ; license_model = Aws.Util.option_map (Aws.Json.lookup j "LicenseModel") String.of_json
  ; availability_zone_group =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneGroup") String.of_json
  ; vpc = Aws.Util.option_map (Aws.Json.lookup j "Vpc") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
