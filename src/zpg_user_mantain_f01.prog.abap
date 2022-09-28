*&---------------------------------------------------------------------*
*& Include          ZPG_USER_MANTAIN_F01
*&---------------------------------------------------------------------*

FORM manage_users USING pv_process_type TYPE int2.

  DATA: lv_process_result    TYPE string,
        lv_technical_message TYPE string.

  CALL FUNCTION 'Z_FM_USER_AUTOMATION'
    EXPORTING
      iv_process           = pv_process_type
      iv_user              = pa_uname
      iv_initial_password  = pn_pass
      iv_days_allowed      = pa_valid
    IMPORTING
      ev_process_result    = lv_process_result
      ev_technical_message = lv_technical_message.

  APPEND INITIAL LINE TO gt_results ASSIGNING FIELD-SYMBOL(<ls_results>).

  <ls_results>-user_name = pa_uname.
  <ls_results>-inicial_password = pn_pass.

  IF lv_process_result(2) EQ 'OK'.

    IF NOT lv_technical_message IS INITIAL.
      <ls_results>-message = lv_technical_message.
    ELSE.
      <ls_results>-message = 'OK'.
    ENDIF.
    <ls_results>-status = gc_green_status..


  ELSE.

    IF NOT lv_technical_message IS INITIAL.
      <ls_results>-message = lv_technical_message.
    ELSE.
      <ls_results>-message = 'KO'.
    ENDIF.
    <ls_results>-status = gc_red_status.

  ENDIF.

ENDFORM.

FORM display_alv.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = DATA(lo_alv_table)
        CHANGING
          t_table        = gt_results ).
    CATCH cx_salv_msg INTO DATA(lx_salv_msg).
      WRITE lx_salv_msg->get_text( ).
  ENDTRY.

  lo_alv_table->get_selections(  )->set_selection_mode( if_salv_c_selection_mode=>multiple ).

  lo_alv_table->display(  ).

ENDFORM.
