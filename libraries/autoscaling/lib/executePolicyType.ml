open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t option
  ; policy_name : String.t
  ; honor_cooldown : Boolean.t option
  ; metric_value : Double.t option
  ; breach_threshold : Double.t option
  }

let make
    ?auto_scaling_group_name
    ~policy_name
    ?honor_cooldown
    ?metric_value
    ?breach_threshold
    () =
  { auto_scaling_group_name; policy_name; honor_cooldown; metric_value; breach_threshold }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
    ; policy_name =
        Aws.Xml.required
          "PolicyName"
          (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
    ; honor_cooldown =
        Aws.Util.option_bind (Aws.Xml.member "HonorCooldown" xml) Boolean.parse
    ; metric_value = Aws.Util.option_bind (Aws.Xml.member "MetricValue" xml) Double.parse
    ; breach_threshold =
        Aws.Util.option_bind (Aws.Xml.member "BreachThreshold" xml) Double.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.breach_threshold (fun f ->
             Aws.Query.Pair ("BreachThreshold", Double.to_query f))
       ; Aws.Util.option_map v.metric_value (fun f ->
             Aws.Query.Pair ("MetricValue", Double.to_query f))
       ; Aws.Util.option_map v.honor_cooldown (fun f ->
             Aws.Query.Pair ("HonorCooldown", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.breach_threshold (fun f ->
             "BreachThreshold", Double.to_json f)
       ; Aws.Util.option_map v.metric_value (fun f -> "MetricValue", Double.to_json f)
       ; Aws.Util.option_map v.honor_cooldown (fun f ->
             "HonorCooldown", Boolean.to_json f)
       ; Some ("PolicyName", String.to_json v.policy_name)
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             "AutoScalingGroupName", String.to_json f)
       ])

let of_json j =
  { auto_scaling_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
  ; policy_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
  ; honor_cooldown =
      Aws.Util.option_map (Aws.Json.lookup j "HonorCooldown") Boolean.of_json
  ; metric_value = Aws.Util.option_map (Aws.Json.lookup j "MetricValue") Double.of_json
  ; breach_threshold =
      Aws.Util.option_map (Aws.Json.lookup j "BreachThreshold") Double.of_json
  }
