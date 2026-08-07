open Aws.BaseTypes

type t =
  { committed_instance_count : Integer.t option
  ; commitment_end_date : DateTime.t option
  }

let make ?committed_instance_count ?commitment_end_date () =
  { committed_instance_count; commitment_end_date }

let parse xml =
  Some
    { committed_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "committedInstanceCount" xml) Integer.parse
    ; commitment_end_date =
        Aws.Util.option_bind (Aws.Xml.member "commitmentEndDate" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.commitment_end_date (fun f ->
             Aws.Query.Pair ("CommitmentEndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.committed_instance_count (fun f ->
             Aws.Query.Pair ("CommittedInstanceCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.commitment_end_date (fun f ->
             "commitmentEndDate", DateTime.to_json f)
       ; Aws.Util.option_map v.committed_instance_count (fun f ->
             "committedInstanceCount", Integer.to_json f)
       ])

let of_json j =
  { committed_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "committedInstanceCount") Integer.of_json
  ; commitment_end_date =
      Aws.Util.option_map (Aws.Json.lookup j "commitmentEndDate") DateTime.of_json
  }
