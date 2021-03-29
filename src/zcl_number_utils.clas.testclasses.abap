CLASS ltcl_number_utils DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: cut TYPE REF TO zcl_number_utils.

    METHODS:
      setup.

    METHODS:
      is_numeric_true FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_number_utils IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD is_numeric_true.

    DATA: lv_value TYPE numc4.

    cut->is_numeric( lv_value ).

    cl_abap_unit_assert=>assert_true(
        act = cut->is_numeric( lv_value )
    ).

  ENDMETHOD.

ENDCLASS.
