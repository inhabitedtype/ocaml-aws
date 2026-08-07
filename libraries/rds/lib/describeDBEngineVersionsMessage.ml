open Aws.BaseTypes

type t =
  { engine : String.t option
  ; engine_version : String.t option
  ; d_b_parameter_group_family : String.t option
  ; filters : FilterList.t
  ; max_records : Integer.t option
  ; marker : String.t option
  ; default_only : Boolean.t option
  ; list_supported_character_sets : Boolean.t option
  ; list_supported_timezones : Boolean.t option
  ; include_all : Boolean.t option
  }

let make
    ?engine
    ?engine_version
    ?d_b_parameter_group_family
    ?(filters = [])
    ?max_records
    ?marker
    ?default_only
    ?list_supported_character_sets
    ?list_supported_timezones
    ?include_all
    () =
  { engine
  ; engine_version
  ; d_b_parameter_group_family
  ; filters
  ; max_records
  ; marker
  ; default_only
  ; list_supported_character_sets
  ; list_supported_timezones
  ; include_all
  }

let parse xml =
  Some
    { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; d_b_parameter_group_family =
        Aws.Util.option_bind (Aws.Xml.member "DBParameterGroupFamily" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; default_only = Aws.Util.option_bind (Aws.Xml.member "DefaultOnly" xml) Boolean.parse
    ; list_supported_character_sets =
        Aws.Util.option_bind
          (Aws.Xml.member "ListSupportedCharacterSets" xml)
          Boolean.parse
    ; list_supported_timezones =
        Aws.Util.option_bind (Aws.Xml.member "ListSupportedTimezones" xml) Boolean.parse
    ; include_all = Aws.Util.option_bind (Aws.Xml.member "IncludeAll" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_all (fun f ->
             Aws.Query.Pair ("IncludeAll", Boolean.to_query f))
       ; Aws.Util.option_map v.list_supported_timezones (fun f ->
             Aws.Query.Pair ("ListSupportedTimezones", Boolean.to_query f))
       ; Aws.Util.option_map v.list_supported_character_sets (fun f ->
             Aws.Query.Pair ("ListSupportedCharacterSets", Boolean.to_query f))
       ; Aws.Util.option_map v.default_only (fun f ->
             Aws.Query.Pair ("DefaultOnly", Boolean.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.d_b_parameter_group_family (fun f ->
             Aws.Query.Pair ("DBParameterGroupFamily", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_all (fun f -> "IncludeAll", Boolean.to_json f)
       ; Aws.Util.option_map v.list_supported_timezones (fun f ->
             "ListSupportedTimezones", Boolean.to_json f)
       ; Aws.Util.option_map v.list_supported_character_sets (fun f ->
             "ListSupportedCharacterSets", Boolean.to_json f)
       ; Aws.Util.option_map v.default_only (fun f -> "DefaultOnly", Boolean.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.d_b_parameter_group_family (fun f ->
             "DBParameterGroupFamily", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ])

let of_json j =
  { engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; d_b_parameter_group_family =
      Aws.Util.option_map (Aws.Json.lookup j "DBParameterGroupFamily") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; default_only = Aws.Util.option_map (Aws.Json.lookup j "DefaultOnly") Boolean.of_json
  ; list_supported_character_sets =
      Aws.Util.option_map (Aws.Json.lookup j "ListSupportedCharacterSets") Boolean.of_json
  ; list_supported_timezones =
      Aws.Util.option_map (Aws.Json.lookup j "ListSupportedTimezones") Boolean.of_json
  ; include_all = Aws.Util.option_map (Aws.Json.lookup j "IncludeAll") Boolean.of_json
  }
