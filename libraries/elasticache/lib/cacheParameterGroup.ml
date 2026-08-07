open Aws.BaseTypes

type t =
  { cache_parameter_group_name : String.t option
  ; cache_parameter_group_family : String.t option
  ; description : String.t option
  ; is_global : Boolean.t option
  ; a_r_n : String.t option
  }

let make
    ?cache_parameter_group_name
    ?cache_parameter_group_family
    ?description
    ?is_global
    ?a_r_n
    () =
  { cache_parameter_group_name
  ; cache_parameter_group_family
  ; description
  ; is_global
  ; a_r_n
  }

let parse xml =
  Some
    { cache_parameter_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
    ; cache_parameter_group_family =
        Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupFamily" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; is_global = Aws.Util.option_bind (Aws.Xml.member "IsGlobal" xml) Boolean.parse
    ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.a_r_n (fun f -> Aws.Query.Pair ("ARN", String.to_query f))
       ; Aws.Util.option_map v.is_global (fun f ->
             Aws.Query.Pair ("IsGlobal", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
             Aws.Query.Pair ("CacheParameterGroupFamily", String.to_query f))
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
       ; Aws.Util.option_map v.is_global (fun f -> "IsGlobal", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
             "CacheParameterGroupFamily", String.to_json f)
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             "CacheParameterGroupName", String.to_json f)
       ])

let of_json j =
  { cache_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
  ; cache_parameter_group_family =
      Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupFamily") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; is_global = Aws.Util.option_map (Aws.Json.lookup j "IsGlobal") Boolean.of_json
  ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
  }
