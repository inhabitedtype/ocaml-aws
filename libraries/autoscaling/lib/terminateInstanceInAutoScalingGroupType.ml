open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; should_decrement_desired_capacity : Boolean.t
  }

let make ~instance_id ~should_decrement_desired_capacity () =
  { instance_id; should_decrement_desired_capacity }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; should_decrement_desired_capacity =
        Aws.Xml.required
          "ShouldDecrementDesiredCapacity"
          (Aws.Util.option_bind
             (Aws.Xml.member "ShouldDecrementDesiredCapacity" xml)
             Boolean.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ShouldDecrementDesiredCapacity"
              , Boolean.to_query v.should_decrement_desired_capacity ))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ShouldDecrementDesiredCapacity"
           , Boolean.to_json v.should_decrement_desired_capacity )
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; should_decrement_desired_capacity =
      Boolean.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ShouldDecrementDesiredCapacity"))
  }
