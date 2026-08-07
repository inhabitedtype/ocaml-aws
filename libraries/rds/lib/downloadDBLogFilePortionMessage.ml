open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; log_file_name : String.t
  ; marker : String.t option
  ; number_of_lines : Integer.t option
  }

let make ~d_b_instance_identifier ~log_file_name ?marker ?number_of_lines () =
  { d_b_instance_identifier; log_file_name; marker; number_of_lines }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
    ; log_file_name =
        Aws.Xml.required
          "LogFileName"
          (Aws.Util.option_bind (Aws.Xml.member "LogFileName" xml) String.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; number_of_lines =
        Aws.Util.option_bind (Aws.Xml.member "NumberOfLines" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.number_of_lines (fun f ->
             Aws.Query.Pair ("NumberOfLines", Integer.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some (Aws.Query.Pair ("LogFileName", String.to_query v.log_file_name))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.number_of_lines (fun f ->
             "NumberOfLines", Integer.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("LogFileName", String.to_json v.log_file_name)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; log_file_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogFileName"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; number_of_lines =
      Aws.Util.option_map (Aws.Json.lookup j "NumberOfLines") Integer.of_json
  }
