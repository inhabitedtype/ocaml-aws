open Aws.BaseTypes

type t =
  { v_p_c_id : String.t
  ; v_p_c_region : VPCRegion.t
  ; max_items : String.t option
  ; next_token : String.t option
  }

let make ~v_p_c_id ~v_p_c_region ?max_items ?next_token () =
  { v_p_c_id; v_p_c_region; max_items; next_token }

let parse xml =
  Some
    { v_p_c_id =
        Aws.Xml.required
          "vpcid"
          (Aws.Util.option_bind (Aws.Xml.member "vpcid" xml) String.parse)
    ; v_p_c_region =
        Aws.Xml.required
          "vpcregion"
          (Aws.Util.option_bind (Aws.Xml.member "vpcregion" xml) VPCRegion.parse)
    ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nexttoken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("nexttoken", String.to_query f))
       ; Aws.Util.option_map v.max_items (fun f ->
             Aws.Query.Pair ("maxitems", String.to_query f))
       ; Some (Aws.Query.Pair ("vpcregion", VPCRegion.to_query v.v_p_c_region))
       ; Some (Aws.Query.Pair ("vpcid", String.to_query v.v_p_c_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nexttoken", String.to_json f)
       ; Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
       ; Some ("vpcregion", VPCRegion.to_json v.v_p_c_region)
       ; Some ("vpcid", String.to_json v.v_p_c_id)
       ])

let of_json j =
  { v_p_c_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcid"))
  ; v_p_c_region =
      VPCRegion.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcregion"))
  ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nexttoken") String.of_json
  }
