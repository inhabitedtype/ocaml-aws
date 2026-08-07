open Aws.BaseTypes

type t =
  { image_id : String.t option
  ; name : String.t option
  ; description : String.t option
  ; recycle_bin_enter_time : DateTime.t option
  ; recycle_bin_exit_time : DateTime.t option
  }

let make ?image_id ?name ?description ?recycle_bin_enter_time ?recycle_bin_exit_time () =
  { image_id; name; description; recycle_bin_enter_time; recycle_bin_exit_time }

let parse xml =
  Some
    { image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; recycle_bin_enter_time =
        Aws.Util.option_bind (Aws.Xml.member "recycleBinEnterTime" xml) DateTime.parse
    ; recycle_bin_exit_time =
        Aws.Util.option_bind (Aws.Xml.member "recycleBinExitTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recycle_bin_exit_time (fun f ->
             Aws.Query.Pair ("RecycleBinExitTime", DateTime.to_query f))
       ; Aws.Util.option_map v.recycle_bin_enter_time (fun f ->
             Aws.Query.Pair ("RecycleBinEnterTime", DateTime.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recycle_bin_exit_time (fun f ->
             "recycleBinExitTime", DateTime.to_json f)
       ; Aws.Util.option_map v.recycle_bin_enter_time (fun f ->
             "recycleBinEnterTime", DateTime.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "name", String.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ])

let of_json j =
  { image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; recycle_bin_enter_time =
      Aws.Util.option_map (Aws.Json.lookup j "recycleBinEnterTime") DateTime.of_json
  ; recycle_bin_exit_time =
      Aws.Util.option_map (Aws.Json.lookup j "recycleBinExitTime") DateTime.of_json
  }
