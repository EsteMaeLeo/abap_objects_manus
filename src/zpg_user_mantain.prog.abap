*&---------------------------------------------------------------------*
*& Report zpg_user_mantain
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpg_user_mantain.

INCLUDE zpg_user_mantain_top.

INCLUDE zpg_user_mantain_sel.

INCLUDE zpg_user_mantain_f01.

START-OF-SELECTION.

  CASE abap_true.
    WHEN pc_user.
      gv_process_type = 1.
    WHEN pu_user.
      gv_process_type = 2.
    WHEN pr_user.
      gv_process_type = 3.
    WHEN pe_user.
      gv_process_type = 4.
    WHEN pl_user.
      gv_process_type = 5.
    WHEN OTHERS.
  ENDCASE.


  IF pa_valid IS INITIAL.
    pa_valid = 100.
  ENDIF.

  PERFORM manage_users USING gv_process_type.

  IF NOT gt_results[] IS INITIAL.
    PERFORM display_alv.
  ENDIF.

END-OF-SELECTION.
