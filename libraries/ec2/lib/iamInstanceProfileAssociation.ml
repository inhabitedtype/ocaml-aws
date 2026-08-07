open Aws.BaseTypes

type t =
  { association_id : String.t option
  ; instance_id : String.t option
  ; iam_instance_profile : IamInstanceProfile.t option
  ; state : IamInstanceProfileAssociationState.t option
  ; timestamp : DateTime.t option
  }

let make ?association_id ?instance_id ?iam_instance_profile ?state ?timestamp () =
  { association_id; instance_id; iam_instance_profile; state; timestamp }

let parse xml =
  Some
    { association_id =
        Aws.Util.option_bind (Aws.Xml.member "associationId" xml) String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; iam_instance_profile =
        Aws.Util.option_bind
          (Aws.Xml.member "iamInstanceProfile" xml)
          IamInstanceProfile.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          IamInstanceProfileAssociationState.parse
    ; timestamp = Aws.Util.option_bind (Aws.Xml.member "timestamp" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", IamInstanceProfileAssociationState.to_query f))
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             Aws.Query.Pair ("IamInstanceProfile", IamInstanceProfile.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.association_id (fun f ->
             Aws.Query.Pair ("AssociationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timestamp (fun f -> "timestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", IamInstanceProfileAssociationState.to_json f)
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             "iamInstanceProfile", IamInstanceProfile.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.association_id (fun f -> "associationId", String.to_json f)
       ])

let of_json j =
  { association_id =
      Aws.Util.option_map (Aws.Json.lookup j "associationId") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; iam_instance_profile =
      Aws.Util.option_map
        (Aws.Json.lookup j "iamInstanceProfile")
        IamInstanceProfile.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        IamInstanceProfileAssociationState.of_json
  ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "timestamp") DateTime.of_json
  }
