FUNCTION z_fm_user_automation.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PROCESS) TYPE  INT2
*"     VALUE(IV_USER) TYPE  XUBNAME
*"     VALUE(IV_INITIAL_PASSWORD) TYPE  XUNCODE OPTIONAL
*"     VALUE(IV_DAYS_ALLOWED) TYPE  INT4 OPTIONAL
*"  EXPORTING
*"     VALUE(EV_PROCESS_RESULT) TYPE  STRING
*"     VALUE(EV_TECHNICAL_MESSAGE) TYPE  STRING
*"----------------------------------------------------------------------
  "1 - Create
  "2- unlock
  "3- Reset password
  "4- Expand Access
  "5- Lock user

  DATA(lo_user_automation) = NEW zcl_user_automation(  ).

  CASE iv_process.

    WHEN 1. "create

      IF lo_user_automation->check_user_exist( iv_user ) EQ abap_true.
        ev_process_result = zcl_user_automation=>er_user_exists.
        EXIT.
      ENDIF.

      IF lo_user_automation->create_user( EXPORTING iv_user             =  iv_user
                                                 iv_initial_password = iv_initial_password
                                                  iv_days_allowed    = iv_days_allowed ) EQ abap_false.

        ev_process_result = zcl_user_automation=>er_user_creation.
        EXIT.

      ENDIF.

      ev_process_result = zcl_user_automation=>ok_create_user.

    WHEN 2. "Unlock user

      IF lo_user_automation->check_user_exist( iv_user ) EQ abap_false.
        ev_process_result = zcl_user_automation=>er_user_doesnot_exist.
        EXIT.
      ELSE.
        IF lo_user_automation->unlock_user( iv_user ) EQ abap_true.
          ev_process_result = zcl_user_automation=>ok_user_lock.
        ELSE.
          ev_process_result = zcl_user_automation=>er_user_unlock_user.
        ENDIF.
      ENDIF.


    WHEN 3. "change user password

      IF lo_user_automation->check_user_exist( iv_user ) EQ abap_false.
        ev_process_result = zcl_user_automation=>er_user_doesnot_exist.
        EXIT.
      ELSE.
        IF lo_user_automation->change_password( iv_user = iv_user
                                                iv_initial_password = iv_initial_password ) EQ abap_true.
          ev_process_result = zcl_user_automation=>ok_changed_password.
        ELSE.
          ev_process_result = zcl_user_automation=>er_user_change_password.
        ENDIF.
      ENDIF.

    WHEN 4. "Expand Access

      IF lo_user_automation->check_user_exist( iv_user ) EQ abap_false.
        ev_process_result = zcl_user_automation=>er_user_doesnot_exist.
        EXIT.
      ELSE.
        IF lo_user_automation->expand_access( iv_user = iv_user
                                                iv_days_allowed = iv_days_allowed ) EQ abap_true.
          ev_process_result = zcl_user_automation=>ok_access_expanded.
        ELSE.
          ev_process_result = zcl_user_automation=>er_user_access_expanded.
        ENDIF.
      ENDIF.

    WHEN 5. "lock Access

      IF lo_user_automation->check_user_exist( iv_user ) EQ abap_false.
        ev_process_result = zcl_user_automation=>er_user_doesnot_exist.
        EXIT.
      ELSE.
        IF lo_user_automation->lock_user( iv_user ) EQ abap_true.
          ev_process_result = zcl_user_automation=>ok_user_lock.
        ELSE.
          ev_process_result = zcl_user_automation=>er_user_user_lock.
        ENDIF.
      ENDIF.

  ENDCASE.

  ev_technical_message = zcl_user_automation=>technical_message.

ENDFUNCTION.
