open Aws.BaseTypes

type t =
  { status : String.t option
  ; e_c2_security_group_name : String.t option
  ; e_c2_security_group_id : String.t option
  ; e_c2_security_group_owner_id : String.t option
  }

let make
    ?status
    ?e_c2_security_group_name
    ?e_c2_security_group_id
    ?e_c2_security_group_owner_id
    () =
  { status
  ; e_c2_security_group_name
  ; e_c2_security_group_id
  ; e_c2_security_group_owner_id
  }

let parse xml =
  Some
    { status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; e_c2_security_group_name =
        Aws.Util.option_bind (Aws.Xml.member "EC2SecurityGroupName" xml) String.parse
    ; e_c2_security_group_id =
        Aws.Util.option_bind (Aws.Xml.member "EC2SecurityGroupId" xml) String.parse
    ; e_c2_security_group_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "EC2SecurityGroupOwnerId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.e_c2_security_group_owner_id (fun f ->
             Aws.Query.Pair ("EC2SecurityGroupOwnerId", String.to_query f))
       ; Aws.Util.option_map v.e_c2_security_group_id (fun f ->
             Aws.Query.Pair ("EC2SecurityGroupId", String.to_query f))
       ; Aws.Util.option_map v.e_c2_security_group_name (fun f ->
             Aws.Query.Pair ("EC2SecurityGroupName", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.e_c2_security_group_owner_id (fun f ->
             "EC2SecurityGroupOwnerId", String.to_json f)
       ; Aws.Util.option_map v.e_c2_security_group_id (fun f ->
             "EC2SecurityGroupId", String.to_json f)
       ; Aws.Util.option_map v.e_c2_security_group_name (fun f ->
             "EC2SecurityGroupName", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ])

let of_json j =
  { status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; e_c2_security_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "EC2SecurityGroupName") String.of_json
  ; e_c2_security_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "EC2SecurityGroupId") String.of_json
  ; e_c2_security_group_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "EC2SecurityGroupOwnerId") String.of_json
  }
