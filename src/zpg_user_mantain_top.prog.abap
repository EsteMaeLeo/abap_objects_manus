*&---------------------------------------------------------------------*
*& Include          ZPG_USER_MANTAIN_TOP
*&---------------------------------------------------------------------*

CONSTANTS: gc_red_status       TYPE icon_d  VALUE '@0A@',
           gc_yellow_status    TYPE icon_d  VALUE '@09@',
           gc_green_status     TYPE icon_d  VALUE '@08@',
           gc_initial_password TYPE xuncode VALUE 'Pass123'.

DATA: gv_process_type TYPE int2,
      gt_results      TYPE TABLE OF zst_results.
