open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t option
  ; client_token : String.t option
  ; instances : InstanceCollections.t
  ; errors : LaunchInstancesErrors.t
  }

let make ?auto_scaling_group_name ?client_token ?(instances = []) ?(errors = []) () =
  { auto_scaling_group_name; client_token; instances; errors }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Instances" xml)
             InstanceCollections.parse)
    ; errors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Errors" xml) LaunchInstancesErrors.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Errors.member", LaunchInstancesErrors.to_query v.errors))
       ; Some
           (Aws.Query.Pair ("Instances.member", InstanceCollections.to_query v.instances))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             Aws.Query.Pair ("AutoScalingGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Errors", LaunchInstancesErrors.to_json v.errors)
       ; Some ("Instances", InstanceCollections.to_json v.instances)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.auto_scaling_group_name (fun f ->
             "AutoScalingGroupName", String.to_json f)
       ])

let of_json j =
  { auto_scaling_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "AutoScalingGroupName") String.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; instances =
      InstanceCollections.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
  ; errors =
      LaunchInstancesErrors.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Errors"))
  }
