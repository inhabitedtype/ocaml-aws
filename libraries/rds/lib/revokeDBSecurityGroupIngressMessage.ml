open Aws.BaseTypes

type t =
  { d_b_security_group_name : String.t
  ; c_i_d_r_i_p : String.t option
  ; e_c2_security_group_name : String.t option
  ; e_c2_security_group_id : String.t option
  ; e_c2_security_group_owner_id : String.t option
  }

let make
    ~d_b_security_group_name
    ?c_i_d_r_i_p
    ?e_c2_security_group_name
    ?e_c2_security_group_id
    ?e_c2_security_group_owner_id
    () =
  { d_b_security_group_name
  ; c_i_d_r_i_p
  ; e_c2_security_group_name
  ; e_c2_security_group_id
  ; e_c2_security_group_owner_id
  }

let parse xml =
  Some
    { d_b_security_group_name =
        Aws.Xml.required
          "DBSecurityGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "DBSecurityGroupName" xml) String.parse)
    ; c_i_d_r_i_p = Aws.Util.option_bind (Aws.Xml.member "CIDRIP" xml) String.parse
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
       ; Aws.Util.option_map v.c_i_d_r_i_p (fun f ->
             Aws.Query.Pair ("CIDRIP", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBSecurityGroupName", String.to_query v.d_b_security_group_name))
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
       ; Aws.Util.option_map v.c_i_d_r_i_p (fun f -> "CIDRIP", String.to_json f)
       ; Some ("DBSecurityGroupName", String.to_json v.d_b_security_group_name)
       ])

let of_json j =
  { d_b_security_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSecurityGroupName"))
  ; c_i_d_r_i_p = Aws.Util.option_map (Aws.Json.lookup j "CIDRIP") String.of_json
  ; e_c2_security_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "EC2SecurityGroupName") String.of_json
  ; e_c2_security_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "EC2SecurityGroupId") String.of_json
  ; e_c2_security_group_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "EC2SecurityGroupOwnerId") String.of_json
  }
