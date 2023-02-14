class ZCL_GL_MFG_UNFAVOURABLE_RESP definition
  public
  final
  create public .

  public section.

    types:
      ty_importerd_data  type zglmfgccpitm,
      tab_importerd_data type standard table of zglmfgccpitm
                         with default key .

    methods constructor
      importing
        !it_data type tab_importerd_data .

    methods send
      returning
        value(result) type abap_bool .

    methods send_and_commit
      returning
        value(result) type abap_bool .

  protected section.

  private section.

    constants:
      begin of gc_text,
        id       type thead-tdid     value 'ST',
        language type thead-tdspras  value 'E',
        object   type thead-tdobject value 'TEXT',
        subject  type thead-tdname   value 'ZGL_QM_MIC_MAIL_UNFAVOUR_SUBJECT',
        body     type thead-tdname   value 'ZGL_QM_MIC_MAIL_UNFAVOUR_BODY',
      end of gc_text .

    types:
      begin of ty_mat_details,
        material    type mara-matnr,
        description type makt-maktx,
        lab_office  type mara-labor,
      end of ty_mat_details .

    data:
      gt_imported_data type tab_importerd_data,
      gs_imported_data type ty_importerd_data.

    methods send_mail_all
      importing
        !iv_commit type abap_bool .

    methods send_mail
      importing
        !is_data type ty_importerd_data .

    methods set_item
      importing
        !is_data type ty_importerd_data .

    methods is_valid_scenario
      returning
        value(result) type abap_bool .

    methods get_subject
      returning
        value(result) type so_obj_des .

    methods get_subject_info
      exporting
        !header type thead
        !lines  type lxe_tline .

    methods set_subject_info
      changing
        !header       type thead
        !lines        type lxe_tline
      returning
        value(result) type so_obj_des .

    methods get_material_details
      returning
        value(result) type ty_mat_details .

    methods get_body
      returning
        value(result) type soli_tab .

    methods get_body_info
      exporting
        !header type thead
        !lines  type lxe_tline .

    methods set_body_info
      changing
        !header       type thead
        !lines        type lxe_tline
      returning
        value(result) type soli_tab .

    methods get_body_hex
      returning
        value(result) type solix_tab .

    methods add_recipient
      changing
        !send type ref to cl_bcs .

    methods get_recipients
      returning
        value(result) type safm_apt_pp_email .

ENDCLASS.



CLASS ZCL_GL_MFG_UNFAVOURABLE_RESP IMPLEMENTATION .


  method constructor .

    me->gt_imported_data = it_data .

  endmethod .


  method send .

    me->send_mail_all( iv_commit = abap_off ) .

  endmethod .


  method send_and_commit .

    me->send_mail_all( iv_commit = abap_on ) .

  endmethod .


  method send_mail_all .

    loop at me->gt_imported_data into data(ls_data) .

      me->send_mail( ls_data ) .

      if ( iv_commit eq abap_true ) .
        commit work .
      endif .

    endloop .

  endmethod.


  method send_mail .

    constants:
      lc_type                     type so_obj_tp value 'RAW',
      lc_importance_high_priority type char1     value '1'.

    data:
      lo_sender type ref to if_sender_bcs.

    " Inform the item that is going to be processed
    me->set_item( is_data = is_data ) .

    if ( me->is_valid_scenario( ) eq abap_false ) .
      return .
    endif .

    " Creates persistent send request
    try .
        data(lo_document) =
          cl_document_bcs=>create_document(
            i_type         = lc_type
            i_subject      = me->get_subject( )
            i_importance   = lc_importance_high_priority
            i_text         = me->get_body( ) ) .
      catch cx_document_bcs . " BCS: Document Exceptions
        return .
    endtry .

    if ( lo_document is not bound ) .
      return .
    endif .

    try .
        data(lo_send_request) = cl_bcs=>create_persistent( ) .
      catch cx_send_req_bcs . " BCS: Send Request Exceptions .
        return .
    endtry .

    if ( lo_send_request is not bound ) .
      return .
    endif .

    " Add document to send request
    try .
        lo_send_request->set_document( i_document = lo_document ) .
      catch cx_send_req_bcs . " BCS: Send Request Exceptions
        return .
    endtry .

    " Get Sender Object
    try .
        lo_send_request->set_sender( i_sender = lo_sender ) .
      catch cx_send_req_bcs . " BCS: Send Request Exceptions
        return .
    endtry .

    me->add_recipient( changing send = lo_send_request ) .


    " Trigger E-Mail immediately
    try .
        lo_send_request->set_send_immediately( i_send_immediately = abap_on ) .
      catch cx_send_req_bcs . " BCS: Send Request Exceptions
    endtry .

    try .
*       data(lv_result) =
          lo_send_request->send( i_with_error_screen = 'X' ) .
      catch cx_send_req_bcs . " BCS: Send Request Exceptions
    endtry .

  endmethod .


  method set_item .

    clear me->gs_imported_data .

    if ( is_data is initial ) .
      return .
    endif .

    me->gs_imported_data = is_data .

  endmethod .


  method is_valid_scenario .

    result = abap_off .

    if ( lines( me->gt_imported_data ) eq 0 ) .
      return .
    endif .

    select count( * )
      from zglmfgccp_unfavo
     where mic_vmap eq me->gs_imported_data-mic_vmap
       and code     eq me->gs_imported_data-code .

    if ( sy-dbcnt gt 0 ) .
      result = abap_on .
    endif .

  endmethod .


  method get_subject .

    me->get_subject_info(
      importing
        header = data(header)
        lines  = data(lines) ) .

    result = me->set_subject_info(
      changing
        header = header
        lines  = lines ) .

  endmethod .


  method get_subject_info .


    call function 'READ_TEXT'
      exporting
        id       = me->gc_text-id
        language = me->gc_text-language
        name     = me->gc_text-subject
        object   = me->gc_text-object
      importing
        header   = header
      tables
        lines    = lines.

  endmethod .


  method set_subject_info .

    constants:
      lc_plant                type p36_pshrt value '&plant&',
      lc_order                type p36_pshrt value '&order&',
      lc_lab_office           type p36_pshrt value '&lab_offi&',
      lc_material_description type p36_pshrt value '&material&'.


*› New CCP Alert +plant+Order+Lab Office+Material Description
*      CCP Alert EH01 11458913 L00 NUTMEG OIL W/ BHA
*      CCP Alert &plant& &order& &lab_offi& &material&

    if ( lines( lines ) eq 0 ) .
      return .
    endif .

    if ( me->gs_imported_data is initial ) .
      return .
    endif .

    data(ls_materiral_details) = me->get_material_details( ) .

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_plant
        value = me->gs_imported_data-werks.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_order
        value = me->gs_imported_data-aufnr.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_lab_office
        value = ls_materiral_details-lab_office.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_material_description
        value = ls_materiral_details-description.

    call function 'TEXT_SYMBOL_REPLACE'
      exporting
        header = header
      tables
        lines  = lines.

    result = value so_obj_des( lines[ 1 ]-tdline optional ) .

  endmethod .


  method get_material_details .

    if ( me->gs_imported_data-matnr is initial ) .
      return .
    endif .

    select a~matnr as material,
           b~maktx as description,
           a~labor as lab_office
      from mara as a
     inner join makt as b
        on a~matnr eq b~matnr
       and b~spras eq @sy-langu
     where a~matnr eq @me->gs_imported_data-matnr
      into @result
     up to 1 rows .
    endselect .

  endmethod .


  method get_body .


    me->get_body_info(
      importing
        header = data(header)
        lines  = data(lines) ) .

    result = me->set_body_info(
      changing
        header = header
        lines  = lines ) .

  endmethod .


  method get_body_info .


    call function 'READ_TEXT'
      exporting
        id       = me->gc_text-id
        language = me->gc_text-language
        name     = me->gc_text-body
        object   = me->gc_text-object
      importing
        header   = header
      tables
        lines    = lines.

  endmethod .


  method set_body_info .

    constants:
      lc_plant                     type p36_pshrt value '&order&',
      lc_lab_office                type p36_pshrt value '&lab_offi&',
      lc_material                  type p36_pshrt value '&material&',
      lc_material_description      type p36_pshrt value '&mat_desc&',
      lc_batch                     type p36_pshrt value '&batch&',
      lc_process_step              type p36_pshrt value '&proc_st1&',
      lc_process_step_description1 type p36_pshrt value '&desc_st1&',
      lc_result_text1              type p36_pshrt value '&rlt_txt1&',
      lc_comment1                  type p36_pshrt value '&cmt1&',
      lc_user_id1                  type p36_pshrt value '&user_id1&',
      lc_time_stamp1               type p36_pshrt value '&tm_stmp1&',

      lc_process_step_description2 type p36_pshrt value '&desc_st2&',
      lc_result_text2              type p36_pshrt value '&rlt_txt2&',
      lc_comment2                  type p36_pshrt value '&cmt2&',
      lc_user_id2                  type p36_pshrt value '&user_id2&',
      lc_time_stamp2               type p36_pshrt value '&tm_stmp2&'.


    if ( lines( lines ) eq 0 ).
      return .
    endif .

    if ( me->gs_imported_data is initial ) .
      return .
    endif .

    data(ls_materiral_details) = me->get_material_details( ) .

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_plant
        value = me->gs_imported_data-werks.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_lab_office
        value = me->gs_imported_data-aufnr.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_material
        value = me->gs_imported_data-matnr.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_material_description
        value = ls_materiral_details-description.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_batch
        value = me->gs_imported_data-charg.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_process_step
        value = me->gs_imported_data-step.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_process_step_description1
        value = me->gs_imported_data-qkurztext.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_result_text1
        value = |result text 1 { me->gs_imported_data-kurztext }|.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_comment1
        value = me->gs_imported_data-werks.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_user_id1
        value = me->gs_imported_data-zccpuser.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_time_stamp1
        value = |{ me->gs_imported_data-zccpts timestamp = user }|.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_process_step_description2
        value = '.'.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_result_text2
        value = '.'.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_comment2
        value = '.'.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_user_id2
        value = '.'.

    call function 'TEXT_SYMBOL_SETVALUE'
      exporting
        name  = lc_time_stamp2
        value = '.'.

    call function 'TEXT_SYMBOL_REPLACE'
      exporting
        header = header
      tables
        lines  = lines.

    result = value soli_tab(
      for l in lines ( line = l-tdline ) ) .

  endmethod .


  method get_body_hex .

    try .
        cl_bcs_convert=>txt_to_solix(
          exporting
            it_soli = me->get_body( )
          importing
           et_solix = result ) .
      catch cx_bcs . " BCS: General Exceptions
        return .
    endtry .

  endmethod .


  method add_recipient .

    if ( send is not bound ) .
      return .
    endif .

    data(lt_recipients) = me->get_recipients( ) .

    if ( lines( lt_recipients ) eq 0 ) .
      return .
    endif .

    loop at lt_recipients into data(ls_data) .

      try .
          data(lv_recipient) =
            cl_cam_address_bcs=>create_internet_address( ls_data ) .
        catch cx_address_bcs .
          continue .
      endtry .

      try .
          send->add_recipient( i_recipient  = lv_recipient ) .
        catch cx_send_req_bcs . " BCS: Send Request Exceptions
      endtry .

    endloop .

  endmethod .


  method get_recipients .

    if ( lines( me->gt_imported_data ) eq 0 ) .
      return .
    endif .

    select werks, mail
      from zglmfgccp_email
      into table @data(lt_data)
     where werks eq @me->gs_imported_data-werks .

    if ( sy-subrc eq 0 ) .
      result =
        value #( for l in lt_data ( conv ad_smtpadr( l-mail ) ) ) .
    endif .

  endmethod .

ENDCLASS.
