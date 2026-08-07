open Aws.BaseTypes

type t =
  { auto_scaling_group_names : AutoScalingGroupNames.t
  ; next_token : String.t option
  ; max_records : Integer.t option
  }

let make ?(auto_scaling_group_names = []) ?next_token ?max_records () =
  { auto_scaling_group_names; next_token; max_records }

let parse xml =
  Some
    { auto_scaling_group_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AutoScalingGroupNames" xml)
             AutoScalingGroupNames.parse)
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
       ; Some
           (Aws.Query.Pair
              ( "AutoScalingGroupNames.member"
              , AutoScalingGroupNames.to_query v.auto_scaling_group_names ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "AutoScalingGroupNames"
           , AutoScalingGroupNames.to_json v.auto_scaling_group_names )
       ])

let of_json j =
  { auto_scaling_group_names =
      AutoScalingGroupNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupNames"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  }
