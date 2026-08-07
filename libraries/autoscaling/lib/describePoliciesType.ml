open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t option
  ; policy_names : PolicyNames.t
  ; policy_types : PolicyTypes.t
  ; next_token : String.t option
  ; max_records : Integer.t option
  }

let make
    ?auto_scaling_group_name
    ?(policy_names = [])
    ?(policy_types = [])
    ?next_token
    ?max_records
    () =
  { auto_scaling_group_name; policy_names; policy_types; next_token; max_records }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
    ; policy_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
    ; policy_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "PolicyTypes" xml) PolicyTypes.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("PolicyTypes.member", PolicyTypes.to_query v.policy_types))
       ; Some (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("PolicyTypes", PolicyTypes.to_json v.policy_types)
       ; Some ("PolicyNames", PolicyNames.to_json v.policy_names)
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             "AutoScalingGroupName", String.to_json f)
       ])

let of_json j =
  { auto_scaling_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
  ; policy_names =
      PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
  ; policy_types =
      PolicyTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyTypes"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  }
