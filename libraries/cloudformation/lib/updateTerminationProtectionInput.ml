open Aws.BaseTypes

type t =
  { enable_termination_protection : Boolean.t
  ; stack_name : String.t
  }

let make ~enable_termination_protection ~stack_name () =
  { enable_termination_protection; stack_name }

let parse xml =
  Some
    { enable_termination_protection =
        Aws.Xml.required
          "EnableTerminationProtection"
          (Aws.Util.option_bind
             (Aws.Xml.member "EnableTerminationProtection" xml)
             Boolean.parse)
    ; stack_name =
        Aws.Xml.required
          "StackName"
          (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
       ; Some
           (Aws.Query.Pair
              ( "EnableTerminationProtection"
              , Boolean.to_query v.enable_termination_protection ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("StackName", String.to_json v.stack_name)
       ; Some
           ("EnableTerminationProtection", Boolean.to_json v.enable_termination_protection)
       ])

let of_json j =
  { enable_termination_protection =
      Boolean.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EnableTerminationProtection"))
  ; stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
  }
