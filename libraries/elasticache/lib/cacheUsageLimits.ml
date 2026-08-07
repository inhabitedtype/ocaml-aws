type t =
  { data_storage : DataStorage.t option
  ; e_c_p_u_per_second : ECPUPerSecond.t option
  }

let make ?data_storage ?e_c_p_u_per_second () = { data_storage; e_c_p_u_per_second }

let parse xml =
  Some
    { data_storage =
        Aws.Util.option_bind (Aws.Xml.member "DataStorage" xml) DataStorage.parse
    ; e_c_p_u_per_second =
        Aws.Util.option_bind (Aws.Xml.member "ECPUPerSecond" xml) ECPUPerSecond.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.e_c_p_u_per_second (fun f ->
             Aws.Query.Pair ("ECPUPerSecond", ECPUPerSecond.to_query f))
       ; Aws.Util.option_map v.data_storage (fun f ->
             Aws.Query.Pair ("DataStorage", DataStorage.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.e_c_p_u_per_second (fun f ->
             "ECPUPerSecond", ECPUPerSecond.to_json f)
       ; Aws.Util.option_map v.data_storage (fun f ->
             "DataStorage", DataStorage.to_json f)
       ])

let of_json j =
  { data_storage =
      Aws.Util.option_map (Aws.Json.lookup j "DataStorage") DataStorage.of_json
  ; e_c_p_u_per_second =
      Aws.Util.option_map (Aws.Json.lookup j "ECPUPerSecond") ECPUPerSecond.of_json
  }
