open Aws.BaseTypes

type t =
  { cores : Integer.t option
  ; instance_type : String.t option
  ; instance_family : String.t option
  ; sockets : Integer.t option
  ; total_v_cpus : Integer.t option
  }

let make ?cores ?instance_type ?instance_family ?sockets ?total_v_cpus () =
  { cores; instance_type; instance_family; sockets; total_v_cpus }

let parse xml =
  Some
    { cores = Aws.Util.option_bind (Aws.Xml.member "cores" xml) Integer.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; instance_family =
        Aws.Util.option_bind (Aws.Xml.member "instanceFamily" xml) String.parse
    ; sockets = Aws.Util.option_bind (Aws.Xml.member "sockets" xml) Integer.parse
    ; total_v_cpus = Aws.Util.option_bind (Aws.Xml.member "totalVCpus" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_v_cpus (fun f ->
             Aws.Query.Pair ("TotalVCpus", Integer.to_query f))
       ; Aws.Util.option_map v.sockets (fun f ->
             Aws.Query.Pair ("Sockets", Integer.to_query f))
       ; Aws.Util.option_map v.instance_family (fun f ->
             Aws.Query.Pair ("InstanceFamily", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.cores (fun f ->
             Aws.Query.Pair ("Cores", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_v_cpus (fun f -> "totalVCpus", Integer.to_json f)
       ; Aws.Util.option_map v.sockets (fun f -> "sockets", Integer.to_json f)
       ; Aws.Util.option_map v.instance_family (fun f ->
             "instanceFamily", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.cores (fun f -> "cores", Integer.to_json f)
       ])

let of_json j =
  { cores = Aws.Util.option_map (Aws.Json.lookup j "cores") Integer.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; instance_family =
      Aws.Util.option_map (Aws.Json.lookup j "instanceFamily") String.of_json
  ; sockets = Aws.Util.option_map (Aws.Json.lookup j "sockets") Integer.of_json
  ; total_v_cpus = Aws.Util.option_map (Aws.Json.lookup j "totalVCpus") Integer.of_json
  }
