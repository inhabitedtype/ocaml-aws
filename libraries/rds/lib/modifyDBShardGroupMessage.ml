open Aws.BaseTypes

type t =
  { d_b_shard_group_identifier : String.t
  ; max_a_c_u : Double.t option
  ; min_a_c_u : Double.t option
  ; compute_redundancy : Integer.t option
  }

let make ~d_b_shard_group_identifier ?max_a_c_u ?min_a_c_u ?compute_redundancy () =
  { d_b_shard_group_identifier; max_a_c_u; min_a_c_u; compute_redundancy }

let parse xml =
  Some
    { d_b_shard_group_identifier =
        Aws.Xml.required
          "DBShardGroupIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "DBShardGroupIdentifier" xml)
             String.parse)
    ; max_a_c_u = Aws.Util.option_bind (Aws.Xml.member "MaxACU" xml) Double.parse
    ; min_a_c_u = Aws.Util.option_bind (Aws.Xml.member "MinACU" xml) Double.parse
    ; compute_redundancy =
        Aws.Util.option_bind (Aws.Xml.member "ComputeRedundancy" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.compute_redundancy (fun f ->
             Aws.Query.Pair ("ComputeRedundancy", Integer.to_query f))
       ; Aws.Util.option_map v.min_a_c_u (fun f ->
             Aws.Query.Pair ("MinACU", Double.to_query f))
       ; Aws.Util.option_map v.max_a_c_u (fun f ->
             Aws.Query.Pair ("MaxACU", Double.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBShardGroupIdentifier", String.to_query v.d_b_shard_group_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.compute_redundancy (fun f ->
             "ComputeRedundancy", Integer.to_json f)
       ; Aws.Util.option_map v.min_a_c_u (fun f -> "MinACU", Double.to_json f)
       ; Aws.Util.option_map v.max_a_c_u (fun f -> "MaxACU", Double.to_json f)
       ; Some ("DBShardGroupIdentifier", String.to_json v.d_b_shard_group_identifier)
       ])

let of_json j =
  { d_b_shard_group_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBShardGroupIdentifier"))
  ; max_a_c_u = Aws.Util.option_map (Aws.Json.lookup j "MaxACU") Double.of_json
  ; min_a_c_u = Aws.Util.option_map (Aws.Json.lookup j "MinACU") Double.of_json
  ; compute_redundancy =
      Aws.Util.option_map (Aws.Json.lookup j "ComputeRedundancy") Integer.of_json
  }
