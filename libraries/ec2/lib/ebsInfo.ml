open Aws.BaseTypes

type t =
  { ebs_optimized_support : EbsOptimizedSupport.t option
  ; encryption_support : EbsEncryptionSupport.t option
  ; ebs_optimized_info : EbsOptimizedInfo.t option
  ; nvme_support : EbsNvmeSupport.t option
  ; maximum_ebs_attachments : Integer.t option
  ; attachment_limit_type : AttachmentLimitType.t option
  ; maximum_ebs_cards : Integer.t option
  ; ebs_cards : EbsCardInfoList.t
  }

let make
    ?ebs_optimized_support
    ?encryption_support
    ?ebs_optimized_info
    ?nvme_support
    ?maximum_ebs_attachments
    ?attachment_limit_type
    ?maximum_ebs_cards
    ?(ebs_cards = [])
    () =
  { ebs_optimized_support
  ; encryption_support
  ; ebs_optimized_info
  ; nvme_support
  ; maximum_ebs_attachments
  ; attachment_limit_type
  ; maximum_ebs_cards
  ; ebs_cards
  }

let parse xml =
  Some
    { ebs_optimized_support =
        Aws.Util.option_bind
          (Aws.Xml.member "ebsOptimizedSupport" xml)
          EbsOptimizedSupport.parse
    ; encryption_support =
        Aws.Util.option_bind
          (Aws.Xml.member "encryptionSupport" xml)
          EbsEncryptionSupport.parse
    ; ebs_optimized_info =
        Aws.Util.option_bind
          (Aws.Xml.member "ebsOptimizedInfo" xml)
          EbsOptimizedInfo.parse
    ; nvme_support =
        Aws.Util.option_bind (Aws.Xml.member "nvmeSupport" xml) EbsNvmeSupport.parse
    ; maximum_ebs_attachments =
        Aws.Util.option_bind (Aws.Xml.member "maximumEbsAttachments" xml) Integer.parse
    ; attachment_limit_type =
        Aws.Util.option_bind
          (Aws.Xml.member "attachmentLimitType" xml)
          AttachmentLimitType.parse
    ; maximum_ebs_cards =
        Aws.Util.option_bind (Aws.Xml.member "maximumEbsCards" xml) Integer.parse
    ; ebs_cards =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ebsCardSet" xml) EbsCardInfoList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("EbsCardSet", EbsCardInfoList.to_query v.ebs_cards))
       ; Aws.Util.option_map v.maximum_ebs_cards (fun f ->
             Aws.Query.Pair ("MaximumEbsCards", Integer.to_query f))
       ; Aws.Util.option_map v.attachment_limit_type (fun f ->
             Aws.Query.Pair ("AttachmentLimitType", AttachmentLimitType.to_query f))
       ; Aws.Util.option_map v.maximum_ebs_attachments (fun f ->
             Aws.Query.Pair ("MaximumEbsAttachments", Integer.to_query f))
       ; Aws.Util.option_map v.nvme_support (fun f ->
             Aws.Query.Pair ("NvmeSupport", EbsNvmeSupport.to_query f))
       ; Aws.Util.option_map v.ebs_optimized_info (fun f ->
             Aws.Query.Pair ("EbsOptimizedInfo", EbsOptimizedInfo.to_query f))
       ; Aws.Util.option_map v.encryption_support (fun f ->
             Aws.Query.Pair ("EncryptionSupport", EbsEncryptionSupport.to_query f))
       ; Aws.Util.option_map v.ebs_optimized_support (fun f ->
             Aws.Query.Pair ("EbsOptimizedSupport", EbsOptimizedSupport.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ebsCardSet", EbsCardInfoList.to_json v.ebs_cards)
       ; Aws.Util.option_map v.maximum_ebs_cards (fun f ->
             "maximumEbsCards", Integer.to_json f)
       ; Aws.Util.option_map v.attachment_limit_type (fun f ->
             "attachmentLimitType", AttachmentLimitType.to_json f)
       ; Aws.Util.option_map v.maximum_ebs_attachments (fun f ->
             "maximumEbsAttachments", Integer.to_json f)
       ; Aws.Util.option_map v.nvme_support (fun f ->
             "nvmeSupport", EbsNvmeSupport.to_json f)
       ; Aws.Util.option_map v.ebs_optimized_info (fun f ->
             "ebsOptimizedInfo", EbsOptimizedInfo.to_json f)
       ; Aws.Util.option_map v.encryption_support (fun f ->
             "encryptionSupport", EbsEncryptionSupport.to_json f)
       ; Aws.Util.option_map v.ebs_optimized_support (fun f ->
             "ebsOptimizedSupport", EbsOptimizedSupport.to_json f)
       ])

let of_json j =
  { ebs_optimized_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "ebsOptimizedSupport")
        EbsOptimizedSupport.of_json
  ; encryption_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "encryptionSupport")
        EbsEncryptionSupport.of_json
  ; ebs_optimized_info =
      Aws.Util.option_map (Aws.Json.lookup j "ebsOptimizedInfo") EbsOptimizedInfo.of_json
  ; nvme_support =
      Aws.Util.option_map (Aws.Json.lookup j "nvmeSupport") EbsNvmeSupport.of_json
  ; maximum_ebs_attachments =
      Aws.Util.option_map (Aws.Json.lookup j "maximumEbsAttachments") Integer.of_json
  ; attachment_limit_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "attachmentLimitType")
        AttachmentLimitType.of_json
  ; maximum_ebs_cards =
      Aws.Util.option_map (Aws.Json.lookup j "maximumEbsCards") Integer.of_json
  ; ebs_cards =
      EbsCardInfoList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ebsCardSet"))
  }
