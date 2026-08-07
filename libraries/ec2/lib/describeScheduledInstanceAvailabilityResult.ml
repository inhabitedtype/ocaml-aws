open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; scheduled_instance_availability_set : ScheduledInstanceAvailabilitySet.t
  }

let make ?next_token ?(scheduled_instance_availability_set = []) () =
  { next_token; scheduled_instance_availability_set }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; scheduled_instance_availability_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "scheduledInstanceAvailabilitySet" xml)
             ScheduledInstanceAvailabilitySet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ScheduledInstanceAvailabilitySet"
              , ScheduledInstanceAvailabilitySet.to_query
                  v.scheduled_instance_availability_set ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "scheduledInstanceAvailabilitySet"
           , ScheduledInstanceAvailabilitySet.to_json
               v.scheduled_instance_availability_set )
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; scheduled_instance_availability_set =
      ScheduledInstanceAvailabilitySet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "scheduledInstanceAvailabilitySet"))
  }
