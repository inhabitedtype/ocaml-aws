open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; record_name : String.t
  ; record_type : RRType.t
  ; resolver_i_p : String.t option
  ; e_d_n_s0_client_subnet_i_p : String.t option
  ; e_d_n_s0_client_subnet_mask : String.t option
  }

let make
    ~hosted_zone_id
    ~record_name
    ~record_type
    ?resolver_i_p
    ?e_d_n_s0_client_subnet_i_p
    ?e_d_n_s0_client_subnet_mask
    () =
  { hosted_zone_id
  ; record_name
  ; record_type
  ; resolver_i_p
  ; e_d_n_s0_client_subnet_i_p
  ; e_d_n_s0_client_subnet_mask
  }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "hostedzoneid"
          (Aws.Util.option_bind (Aws.Xml.member "hostedzoneid" xml) String.parse)
    ; record_name =
        Aws.Xml.required
          "recordname"
          (Aws.Util.option_bind (Aws.Xml.member "recordname" xml) String.parse)
    ; record_type =
        Aws.Xml.required
          "recordtype"
          (Aws.Util.option_bind (Aws.Xml.member "recordtype" xml) RRType.parse)
    ; resolver_i_p = Aws.Util.option_bind (Aws.Xml.member "resolverip" xml) String.parse
    ; e_d_n_s0_client_subnet_i_p =
        Aws.Util.option_bind (Aws.Xml.member "edns0clientsubnetip" xml) String.parse
    ; e_d_n_s0_client_subnet_mask =
        Aws.Util.option_bind (Aws.Xml.member "edns0clientsubnetmask" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.e_d_n_s0_client_subnet_mask (fun f ->
             Aws.Query.Pair ("edns0clientsubnetmask", String.to_query f))
       ; Aws.Util.option_map v.e_d_n_s0_client_subnet_i_p (fun f ->
             Aws.Query.Pair ("edns0clientsubnetip", String.to_query f))
       ; Aws.Util.option_map v.resolver_i_p (fun f ->
             Aws.Query.Pair ("resolverip", String.to_query f))
       ; Some (Aws.Query.Pair ("recordtype", RRType.to_query v.record_type))
       ; Some (Aws.Query.Pair ("recordname", String.to_query v.record_name))
       ; Some (Aws.Query.Pair ("hostedzoneid", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.e_d_n_s0_client_subnet_mask (fun f ->
             "edns0clientsubnetmask", String.to_json f)
       ; Aws.Util.option_map v.e_d_n_s0_client_subnet_i_p (fun f ->
             "edns0clientsubnetip", String.to_json f)
       ; Aws.Util.option_map v.resolver_i_p (fun f -> "resolverip", String.to_json f)
       ; Some ("recordtype", RRType.to_json v.record_type)
       ; Some ("recordname", String.to_json v.record_name)
       ; Some ("hostedzoneid", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "hostedzoneid"))
  ; record_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "recordname"))
  ; record_type = RRType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "recordtype"))
  ; resolver_i_p = Aws.Util.option_map (Aws.Json.lookup j "resolverip") String.of_json
  ; e_d_n_s0_client_subnet_i_p =
      Aws.Util.option_map (Aws.Json.lookup j "edns0clientsubnetip") String.of_json
  ; e_d_n_s0_client_subnet_mask =
      Aws.Util.option_map (Aws.Json.lookup j "edns0clientsubnetmask") String.of_json
  }
