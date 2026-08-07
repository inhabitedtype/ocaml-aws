open Aws.BaseTypes

type t =
  { stack_set_name : String.t
  ; stack_instance_account : String.t
  ; stack_instance_region : String.t
  ; call_as : CallAs.t option
  }

let make ~stack_set_name ~stack_instance_account ~stack_instance_region ?call_as () =
  { stack_set_name; stack_instance_account; stack_instance_region; call_as }

let parse xml =
  Some
    { stack_set_name =
        Aws.Xml.required
          "StackSetName"
          (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
    ; stack_instance_account =
        Aws.Xml.required
          "StackInstanceAccount"
          (Aws.Util.option_bind (Aws.Xml.member "StackInstanceAccount" xml) String.parse)
    ; stack_instance_region =
        Aws.Xml.required
          "StackInstanceRegion"
          (Aws.Util.option_bind (Aws.Xml.member "StackInstanceRegion" xml) String.parse)
    ; call_as = Aws.Util.option_bind (Aws.Xml.member "CallAs" xml) CallAs.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f ->
             Aws.Query.Pair ("CallAs", CallAs.to_query f))
       ; Some
           (Aws.Query.Pair ("StackInstanceRegion", String.to_query v.stack_instance_region))
       ; Some
           (Aws.Query.Pair
              ("StackInstanceAccount", String.to_query v.stack_instance_account))
       ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.call_as (fun f -> "CallAs", CallAs.to_json f)
       ; Some ("StackInstanceRegion", String.to_json v.stack_instance_region)
       ; Some ("StackInstanceAccount", String.to_json v.stack_instance_account)
       ; Some ("StackSetName", String.to_json v.stack_set_name)
       ])

let of_json j =
  { stack_set_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
  ; stack_instance_account =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackInstanceAccount"))
  ; stack_instance_region =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackInstanceRegion"))
  ; call_as = Aws.Util.option_map (Aws.Json.lookup j "CallAs") CallAs.of_json
  }
