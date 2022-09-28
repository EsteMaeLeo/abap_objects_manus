CLASS zcl_zautouser_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_zautouser_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    METHODS /iwbep/if_mgw_appl_srv_runtime~execute_action REDEFINITION.
  PROTECTED SECTION.
    METHODS userset_get_entity REDEFINITION.
    METHODS userset_get_entityset REDEFINITION.
    METHODS userset_create_entity REDEFINITION.
    METHODS userset_delete_entity REDEFINITION.
  PRIVATE SECTION.
    DATA mo_data_provider TYPE REF TO zcl_user_automation.

    CONSTANTS cv_initial_pass TYPE xuncode VALUE 'Pass123'.
    CONSTANTS cv_default_days TYPE int4    VALUE 100.
    CONSTANTS cv_fi_lock_user TYPE string  VALUE 'LockUser'.
ENDCLASS.



CLASS zcl_zautouser_dpc_ext IMPLEMENTATION.

  METHOD constructor.

    super->constructor(  ).

    mo_data_provider = NEW zcl_user_automation(  ).


  ENDMETHOD.


  METHOD userset_get_entity.

    DATA lv_user TYPE xubname.

    lv_user = it_key_tab[ name = 'UserId' ]-value.

    IF mo_data_provider->user_details( EXPORTING iv_user       = to_upper( lv_user )
                                       IMPORTING es_user_data  = er_entity  ) EQ abap_false.

      DATA(ls_message) = VALUE scx_t100key( msgid = 'SY'
                                            msgno = '002'
                                            attr1 = zcl_user_automation=>technical_message ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING textid = ls_message.

    ENDIF.


  ENDMETHOD.


  METHOD userset_get_entityset.
    DATA lv_user TYPE xubname.

    "lv_user = it_key_tab[ name = 'UserId' ]-value.

    IF mo_data_provider->user_details( EXPORTING iv_all_users  = abap_true
                                       IMPORTING et_users_data = et_entityset  ) EQ abap_false.

      DATA(ls_message) = VALUE scx_t100key( msgid = 'SY'
                                            msgno = '002'
                                            attr1 = zcl_user_automation=>technical_message ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING textid = ls_message.

    ENDIF.
  ENDMETHOD.

  METHOD userset_create_entity.


    DATA ls_users TYPE zst_manage_usr.

    "getting data body request

    io_data_provider->read_entry_data( IMPORTING es_data = ls_users ).

    IF mo_data_provider->check_user_exist( to_upper( ls_users-user_id ) ) EQ abap_true.

      DATA(ls_message) = VALUE scx_t100key( msgid = 'SY'
                                            msgno = '002'
                                            attr1 = zcl_user_automation=>technical_message ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING textid = ls_message.

    ELSE.
      "Create user
      IF mo_data_provider->create_user( EXPORTING iv_user             = to_upper( ls_users-user_id )
                                                  is_user_details     = ls_users
                                                  iv_initial_password = cv_initial_pass
                                                  iv_days_allowed     = cv_default_days )  EQ abap_true.

        IF mo_data_provider->user_details( EXPORTING iv_user       = to_upper( ls_users-user_id )
                                           IMPORTING es_user_data  = er_entity  ) EQ abap_false.

          ls_message = VALUE scx_t100key( msgid = 'SY'
                                          msgno = '002'
                                          attr1 = zcl_user_automation=>technical_message ).

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING textid = ls_message.

        ENDIF.

      ELSE.
        ls_message = VALUE scx_t100key( msgid = 'SY'
                                        msgno = '002'
                                        attr1 = zcl_user_automation=>technical_message ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING textid = ls_message.

      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD userset_delete_entity.

    DATA: lt_return TYPE TABLE OF bapiret2,
          ls_users  TYPE zst_manage_usr,
          lv_user   TYPE xubname.


    DATA(lt_keys) = io_tech_request_context->get_keys(  ).

    lv_user = to_upper( lt_keys[ name = 'USER_ID' ]-value ).

    IF mo_data_provider->check_user_exist( to_upper( lv_user ) ) EQ abap_true.


      IF mo_data_provider->delete_user( lv_user ) EQ abap_false.

        DATA(ls_message) = VALUE scx_t100key( msgid = 'SY'
                                              msgno = '002'
                                              attr1 = zcl_user_automation=>technical_message ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING textid = ls_message.

      ENDIF.

    ELSE.

      ls_message = VALUE scx_t100key( msgid = 'SY'
                                      msgno = '002'
                                      attr1 = zcl_user_automation=>technical_message ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING textid = ls_message.

    ENDIF.

  ENDMETHOD.



  METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.

    CASE iv_action_name.
      WHEN cv_fi_lock_user.

        DATA: lv_user      TYPE xubname,
              lv_user_lock TYPE abap_bool.

        lv_user = to_upper( it_parameter[ name = 'UserId' ]-value ).

        IF mo_data_provider->check_user_exist( lv_user ) EQ abap_true.

          IF mo_data_provider->lock_user( lv_user ) EQ abap_true.
            lv_user_lock = abap_true.
          ENDIF.

        ENDIF.

*        IF lv_user_lock EQ abap_true.
*
*          DATA(ls_entity) = VALUE zCL_ZAUTOUSER_MPC=>ts_return( success = abap_true
*                                                                error  = abap_false
*                                                                message = zcl_user_automation=>technical_message ).
*        ELSE.
*          ls_entity = VALUE zCL_ZAUTOUSER_MPC=>ts_return( success = abap_false
*                                                          error  = abap_true
*                                                          message = zcl_user_automation=>technical_message ).
*        ENDIF.

        DATA(ls_entity) = VALUE zCL_ZAUTOUSER_MPC=>ts_return( success = COND #( WHEN lv_user_lock EQ abap_true THEN abap_true
                                                                                ELSE abap_false )
                                                              error  = COND #( WHEN lv_user_lock EQ abap_false THEN abap_true
                                                                                ELSE abap_false )
                                                              message = zcl_user_automation=>technical_message ).

        me->copy_data_to_ref( EXPORTING is_data = ls_entity
                              CHANGING  cr_data = er_data ).

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
