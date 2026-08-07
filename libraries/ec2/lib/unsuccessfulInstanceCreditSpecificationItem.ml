open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; error : UnsuccessfulInstanceCreditSpecificationItemError.t option
  }

let make ?instance_id ?error () = { instance_id; error }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; error =
        Aws.Util.option_bind
          (Aws.Xml.member "error" xml)
          UnsuccessfulInstanceCreditSpecificationItemError.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error (fun f ->
             Aws.Query.Pair
               ("Error", UnsuccessfulInstanceCreditSpecificationItemError.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error (fun f ->
             "error", UnsuccessfulInstanceCreditSpecificationItemError.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; error =
      Aws.Util.option_map
        (Aws.Json.lookup j "error")
        UnsuccessfulInstanceCreditSpecificationItemError.of_json
  }
