CLASS zcl_user_automation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPE-POOLS abap.
    CONSTANTS ok_create_user      TYPE char4 VALUE 'OK01'.
    CONSTANTS ok_user_lock        TYPE char4 VALUE 'OK02'.
    CONSTANTS ok_changed_password TYPE char4 VALUE 'OK03'.
    CONSTANTS ok_access_expanded  TYPE char4 VALUE 'OK04'.
    CONSTANTS ok_user_locked      TYPE char4 VALUE 'OK05'.

    CONSTANTS er_user_exists          TYPE char4 VALUE 'ER01'.
    CONSTANTS er_user_doesnot_exist   TYPE char4 VALUE 'ER02'.
    CONSTANTS er_user_creation        TYPE char4 VALUE 'ER03'.
    CONSTANTS er_user_unlock_user     TYPE char4 VALUE 'ER04'.
    CONSTANTS er_user_change_password TYPE char4 VALUE 'ER05'.
    CONSTANTS er_user_access_expanded TYPE char4 VALUE 'ER06'.
    CONSTANTS er_user_unknown_process TYPE char4 VALUE 'ER07'.
    CONSTANTS er_user_user_lock       TYPE char4 VALUE 'ER08'.

    CLASS-DATA technical_message TYPE string.

    METHODS check_user_exist IMPORTING iv_user          TYPE xubname
                             RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS create_user IMPORTING iv_user             TYPE xubname
                                  iv_initial_password TYPE xuncode
                                  is_user_details     TYPE zst_manage_usr OPTIONAL
                                  iv_days_allowed     TYPE int4
                        EXPORTING ev_expiration_date  TYPE sydatum
                        RETURNING VALUE(rv_result)    TYPE abap_bool.

    METHODS commit_work.

    METHODS set_bapi_message IMPORTING it_return_table TYPE bapirettab.

    METHODS unlock_user IMPORTING iv_user          TYPE xubname
                        RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS change_password  IMPORTING iv_user             TYPE xubname
                                       iv_initial_password TYPE xuncode
                             RETURNING VALUE(rv_result)    TYPE abap_bool.

    METHODS expand_access IMPORTING iv_user            TYPE xubname
                                    iv_days_allowed    TYPE int4
                          EXPORTING ev_expiration_date TYPE sydatum
                          RETURNING VALUE(rv_result)   TYPE abap_bool.

    METHODS lock_user IMPORTING iv_user          TYPE xubname
                      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS user_details IMPORTING iv_user          TYPE xubname   OPTIONAL
                                   iv_all_users     TYPE abap_bool OPTIONAL
                                   iv_top           TYPE i         OPTIONAL
                         EXPORTING es_user_data     TYPE zst_manage_usr
                                   et_users_data    TYPE ztt_manage_usrr
                         RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS delete_user IMPORTING iv_user          TYPE xubname
                        RETURNING VALUE(rv_result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_user_details IMPORTING iv_user          TYPE xubname
                             EXPORTING es_user_data     TYPE zst_manage_usr
                             RETURNING VALUE(rv_result) TYPE abap_bool.
ENDCLASS.



CLASS zcl_user_automation IMPLEMENTATION.

  METHOD check_user_exist.

    CLEAR technical_message.

    SELECT SINGLE FROM usr02
           FIELDS bname
           WHERE bname EQ @iv_user
           INTO @DATA(lv_user).

    IF sy-subrc EQ 0.
      rv_result = abap_true.
      technical_message = 'The SAP user exist in SAP'.
    ELSE.
      technical_message = 'The SAP user doesnt exists in SAP'.
    ENDIF.

  ENDMETHOD.

  METHOD create_user.

    DATA: ls_logondata TYPE bapilogond,
          ls_bapiaddr3 TYPE bapiaddr3,
          lt_return    TYPE bapirettab.

    CLEAR technical_message.

    ev_expiration_date = sy-datum + iv_days_allowed.

    "logon data
    ls_logondata-accnt = iv_user.
    ls_logondata-gltgb = ev_expiration_date.

    "user data
    ls_bapiaddr3-firstname  = is_user_details-firstname.
    ls_bapiaddr3-lastname   = is_user_details-lastname.
    ls_bapiaddr3-fullname   = is_user_details-fullname.
    ls_bapiaddr3-title_aca1 = is_user_details-title_aca1.
    ls_bapiaddr3-langu      = is_user_details-langu_p.
    ls_bapiaddr3-pers_no    = is_user_details-pers_no.
    ls_bapiaddr3-addr_no    = is_user_details-addr_no.

    CALL FUNCTION 'BAPI_USER_CREATE1'
      EXPORTING
        username  = iv_user
        logondata = ls_logondata
        password  = iv_initial_password
        address   = ls_bapiaddr3
      TABLES
        return    = lt_return.


    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.

    IF sy-subrc NE 0.
      rv_result = abap_true.
      me->commit_work(  ).
    ELSE.
      CLEAR ev_expiration_date.
    ENDIF.

    me->set_bapi_message( lt_return ).

  ENDMETHOD.

  METHOD commit_work.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDMETHOD.

  METHOD set_bapi_message.

    CHECK it_return_table IS NOT INITIAL.

    READ TABLE it_return_table INTO DATA(ls_return) INDEX 1.

    IF sy-subrc EQ 0.
      technical_message = |{ ls_return-message }| &&
                          |{ ls_return-message_v1 }| &&
                          |{ ls_return-message_v2 }| &&
                          |{ ls_return-message_v3 }| &&
                          |{ ls_return-message_v4 }|.
    ENDIF.

  ENDMETHOD.

  METHOD unlock_user.

    DATA  lt_return    TYPE bapirettab.

    CLEAR technical_message.

    CALL FUNCTION 'BAPI_USER_UNLOCK'
      EXPORTING
        username = iv_user
      TABLES
        return   = lt_return.

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.

    IF sy-subrc NE 0.
      rv_result = abap_true.
      me->commit_work(  ).

    ENDIF.

    me->set_bapi_message( lt_return ).

  ENDMETHOD.

  METHOD change_password.

    DATA: lt_return    TYPE bapiret2_tab,
          ls_password  TYPE bapipwd,
          ls_passwordx TYPE bapipwdx.

    CLEAR technical_message.

    ls_password = iv_initial_password.
    ls_passwordx = abap_true.

    CALL FUNCTION 'BAPI_USER_CHANGE'
      EXPORTING
        username  = iv_user
        password  = ls_password
        passwordx = ls_passwordx
      TABLES
        return    = lt_return.

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.

    IF sy-subrc NE 0.
      me->commit_work(  ).


      IF me->unlock_user( iv_user ) EQ abap_true.
        rv_result = abap_true.
      ENDIF.


    ENDIF.

    me->set_bapi_message( lt_return ).

  ENDMETHOD.

  METHOD expand_access.

    DATA: lt_return     TYPE bapiret2_tab,
          ls_logondata  TYPE bapilogond,
          ls_logondatax TYPE bapilogonx.

    CLEAR technical_message.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username  = iv_user
      IMPORTING
        logondata = ls_logondata
      TABLES
        return    = lt_return.

    IF ls_logondata-gltgb LT sy-datum.
      ev_expiration_date = sy-datum + iv_days_allowed.
    ELSE.
      ev_expiration_date = ls_logondata-gltgb + iv_days_allowed.
    ENDIF.

    ls_logondata-gltgb = ev_expiration_date.
    ls_logondatax-gltgb = abap_true.

    CALL FUNCTION 'BAPI_USER_CHANGE'
      EXPORTING
        username   = iv_user
        logondata  = ls_logondata
        logondatax = ls_logondatax
      TABLES
        return     = lt_return.

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.

    IF sy-subrc NE 0.
      rv_result = abap_true.
      me->commit_work(  ).

    ENDIF.

    me->set_bapi_message( lt_return ).

  ENDMETHOD.

  METHOD lock_user.
    DATA  lt_return    TYPE bapirettab.

    CLEAR technical_message.

    CALL FUNCTION 'BAPI_USER_LOCK'
      EXPORTING
        username = iv_user
      TABLES
        return   = lt_return.

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.

    IF sy-subrc NE 0.
      rv_result = abap_true.
      me->commit_work(  ).

    ENDIF.

    me->set_bapi_message( lt_return ).

  ENDMETHOD.

  METHOD user_details.

    DATA lt_users_data TYPE ztt_manage_usrr.

    CLEAR technical_message.

    IF iv_all_users EQ abap_true.

      "Get all users
      SELECT FROM usr02
              FIELDS bname AS user_id,
                     ustyp AS user_type,
                     class AS user_class,
                     aname AS creator,
                     erdat AS creation_date,
                     pwdchgdate AS change_pass_date
              INTO CORRESPONDING FIELDS OF TABLE @lt_users_data.

      IF sy-subrc EQ 0.
        rv_result = abap_true.
        LOOP AT lt_users_data ASSIGNING FIELD-SYMBOL(<lfs_user_data>).
          me->get_user_details(
            EXPORTING
              iv_user      =  <lfs_user_data>-user_id
            IMPORTING
              es_user_data = <lfs_user_data> ).

          APPEND <lfs_user_data> TO et_users_data.

        ENDLOOP.
      ENDIF.

    ELSE.
      "Get only one user
      SELECT SINGLE FROM usr02
             FIELDS bname AS user_id,
                    ustyp AS user_type,
                    class AS user_class,
                    aname AS creator,
                    erdat AS creation_date,
                    pwdchgdate AS change_pass_date
             WHERE bname EQ @iv_user
             INTO CORRESPONDING FIELDS OF @es_user_data.

      rv_result = me->get_user_details( EXPORTING iv_user      = iv_user
                                        IMPORTING es_user_data = es_user_data ).

    ENDIF.

  ENDMETHOD.

  METHOD get_user_details.

    DATA: lt_return  TYPE bapiret2_tab,
          ls_address TYPE bapiaddr3.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = iv_user
      IMPORTING
        address  = ls_address
      TABLES
        return   = lt_return.

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.

    IF sy-subrc NE 0.
      rv_result = abap_true.
      MOVE-CORRESPONDING ls_address TO es_user_data.
      me->commit_work(  ).
    ENDIF.

    me->set_bapi_message( it_return_table = lt_return ).

  ENDMETHOD.

  METHOD delete_user.

    DATA: lt_return  TYPE  TABLE OF bapiret2.

    CALL FUNCTION 'BAPI_USER_DELETE'
      EXPORTING
        username = iv_user
      TABLES
        return   = lt_return.

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.

    IF sy-subrc NE 0.
      rv_result = abap_true.
      me->commit_work(  ).
    ENDIF.

    me->set_bapi_message( it_return_table = lt_return ).


  ENDMETHOD.

ENDCLASS.
