open Aws.BaseTypes

type t =
  { public_ip : String.t option
  ; allocation_id : String.t option
  ; ptr_record : String.t option
  ; ptr_record_update : PtrUpdateStatus.t option
  }

let make ?public_ip ?allocation_id ?ptr_record ?ptr_record_update () =
  { public_ip; allocation_id; ptr_record; ptr_record_update }

let parse xml =
  Some
    { public_ip = Aws.Util.option_bind (Aws.Xml.member "publicIp" xml) String.parse
    ; allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "allocationId" xml) String.parse
    ; ptr_record = Aws.Util.option_bind (Aws.Xml.member "ptrRecord" xml) String.parse
    ; ptr_record_update =
        Aws.Util.option_bind (Aws.Xml.member "ptrRecordUpdate" xml) PtrUpdateStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ptr_record_update (fun f ->
             Aws.Query.Pair ("PtrRecordUpdate", PtrUpdateStatus.to_query f))
       ; Aws.Util.option_map v.ptr_record (fun f ->
             Aws.Query.Pair ("PtrRecord", String.to_query f))
       ; Aws.Util.option_map v.allocation_id (fun f ->
             Aws.Query.Pair ("AllocationId", String.to_query f))
       ; Aws.Util.option_map v.public_ip (fun f ->
             Aws.Query.Pair ("PublicIp", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ptr_record_update (fun f ->
             "ptrRecordUpdate", PtrUpdateStatus.to_json f)
       ; Aws.Util.option_map v.ptr_record (fun f -> "ptrRecord", String.to_json f)
       ; Aws.Util.option_map v.allocation_id (fun f -> "allocationId", String.to_json f)
       ; Aws.Util.option_map v.public_ip (fun f -> "publicIp", String.to_json f)
       ])

let of_json j =
  { public_ip = Aws.Util.option_map (Aws.Json.lookup j "publicIp") String.of_json
  ; allocation_id = Aws.Util.option_map (Aws.Json.lookup j "allocationId") String.of_json
  ; ptr_record = Aws.Util.option_map (Aws.Json.lookup j "ptrRecord") String.of_json
  ; ptr_record_update =
      Aws.Util.option_map (Aws.Json.lookup j "ptrRecordUpdate") PtrUpdateStatus.of_json
  }
