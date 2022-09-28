*&---------------------------------------------------------------------*
*& Include          ZPG_USER_MANTAIN_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK block_01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS pc_user  TYPE xfeld RADIOBUTTON GROUP oper DEFAULT 'X'. "Create User
  PARAMETERS pu_user  TYPE xfeld RADIOBUTTON GROUP oper. "Unlock  User
  PARAMETERS pr_user  TYPE xfeld RADIOBUTTON GROUP oper. "Reset password
  PARAMETERS pe_user  TYPE xfeld RADIOBUTTON GROUP oper. "Expand user
  PARAMETERS pl_user  TYPE xfeld RADIOBUTTON GROUP oper. "Lock user
  PARAMETERS pn_pass  TYPE xuncode DEFAULT gc_initial_password.

SELECTION-SCREEN END OF BLOCK block_01.

SELECTION-SCREEN BEGIN OF BLOCK block_02 WITH FRAME TITLE TEXT-b02.
  PARAMETERS pa_uname TYPE uname OBLIGATORY.
  PARAMETERS pa_valid TYPE int4.
SELECTION-SCREEN END OF BLOCK block_02.
