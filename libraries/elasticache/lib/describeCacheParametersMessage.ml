open Aws.BaseTypes

type t =
  { cache_parameter_group_name : String.t
  ; source : String.t option
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make ~cache_parameter_group_name ?source ?max_records ?marker () =
  { cache_parameter_group_name; source; max_records; marker }

let parse xml =
  Some
    { cache_parameter_group_name =
        Aws.Xml.required
          "CacheParameterGroupName"
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheParameterGroupName" xml)
             String.parse)
    ; source = Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse
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
       ; Aws.Util.option_map v.source (fun f ->
             Aws.Query.Pair ("Source", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CacheParameterGroupName", String.to_query v.cache_parameter_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.source (fun f -> "Source", String.to_json f)
       ; Some ("CacheParameterGroupName", String.to_json v.cache_parameter_group_name)
       ])

let of_json j =
  { cache_parameter_group_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheParameterGroupName"))
  ; source = Aws.Util.option_map (Aws.Json.lookup j "Source") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
