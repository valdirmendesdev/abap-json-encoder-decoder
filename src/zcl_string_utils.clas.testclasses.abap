CLASS ltcl_string_utils DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF test_instance,
        input    TYPE string,
        expected TYPE string,
      END OF test_instance,

      test_instances TYPE STANDARD TABLE OF test_instance.

    DATA: cut   TYPE REF TO zcl_string_utils,
          tests TYPE test_instances.

    METHODS:
      setup.

    METHODS:
      lower_camel                       FOR TESTING RAISING cx_static_check,
      lower_case                        FOR TESTING RAISING cx_static_check,
      remove_accented_chars             FOR TESTING RAISING cx_static_check,
      if_value                          FOR TESTING RAISING cx_static_check,
      if_is_not_initial                 FOR TESTING RAISING cx_static_check,
      remove_special_characters         FOR TESTING RAISING cx_static_check,
      remove_spaces_at_end              FOR TESTING RAISING cx_static_check,
      trim                              FOR TESTING RAISING cx_static_check,
      split_in_table                    FOR TESTING RAISING cx_static_check,
      to_xstring                        FOR TESTING RAISING cx_static_check,
      to_binary                         FOR TESTING RAISING cx_static_check,
      replace_characters                FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_string_utils IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    FREE: tests.
  ENDMETHOD.

  METHOD lower_camel.


    tests = VALUE #(
        ( input = 'foo-bar'                      expected = 'fooBar' )
        ( input = ''                             expected = '' )
        ( input = 'Any:Kind of_string'           expected = 'anyKindOfString' )
        ( input = 'Any_Kind.of-string~type/text' expected = 'anyKindOfStringTypeText' )
        ( input = 'ID'                           expected = 'id' ) ).

    LOOP AT tests ASSIGNING FIELD-SYMBOL(<test>).

      cl_abap_unit_assert=>assert_equals(
        msg = 'Result is not correct'
        exp = <test>-expected
        act = cut->to_lower_camel( input = <test>-input )
      ).

    ENDLOOP.

  ENDMETHOD.

  METHOD lower_case.

    tests = VALUE #(
        ( input = 'FOO-BAR' expected = 'foo-bar' )
        ( input = 'FooBar'  expected = 'foobar' ) ).

    LOOP AT tests ASSIGNING FIELD-SYMBOL(<test>).

      cl_abap_unit_assert=>assert_equals(
        msg = 'Result is not correct'
        exp = <test>-expected
        act = cut->to_lower_case( input = <test>-input )
      ).

    ENDLOOP.

  ENDMETHOD.

  METHOD remove_accented_chars.

    tests = VALUE #(
        ( input = 'Produção' expected = 'Producao' ) ).

    LOOP AT tests ASSIGNING FIELD-SYMBOL(<test>).
      cl_abap_unit_assert=>assert_equals(
        EXPORTING
          act = cut->remove_accented_chars( input = <test>-input )
          exp = <test>-expected
          msg = 'Result is not correct'
      ).
    ENDLOOP.

  ENDMETHOD.

  METHOD if_is_not_initial.

    DATA(value) = 'NOT_INITIAL'.

    DATA(result) = cut->if_is_not_initial( input = value ).

    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act              = result
        msg              =  'Variable is initial').



  ENDMETHOD.

  METHOD if_value.

    DATA: date   TYPE dats,
          result TYPE string.

    date = sy-datlo.
    result = cut->if_value( input =  date ).

    cl_abap_unit_assert=>assert_not_initial(
      EXPORTING
        act              = result
        msg              = 'Variable not contains value').

  ENDMETHOD.

  METHOD remove_special_characters.
    DATA: normal_text     TYPE string VALUE 'Normal',
          specharsear(50) VALUE ''' < > ! " & / = ? : ; , . - ( ) # # % ^ $ | ~ @ ',
          result          TYPE string.

    CONCATENATE normal_text specharsear INTO specharsear.
    result = cut->remove_special_characters( input = specharsear ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act              = result
        exp = normal_text
        msg              = 'Variable contain special character.').

  ENDMETHOD.

  METHOD remove_spaces_at_end.

    DATA: value TYPE string.
    value = '1.0000 '.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = cut->trim( value )
        exp = '1.0000'
        msg = 'Variable ending with space.').

  ENDMETHOD.

  METHOD trim.

    DATA(input) = ' remove side spaces '.
    DATA(result) = cut->trim( input = input ).
    cl_abap_unit_assert=>assert_equals( exp = 'remove side spaces' act = result ).


  ENDMETHOD.

  METHOD split_in_table.

    DATA: input  TYPE string,
          result TYPE string_t.

    input = 'primeira;segunda'.
    result = cut->split_in_table( input = input separator = ';' ).

    READ TABLE result INTO DATA(primeira) INDEX 1.
    READ TABLE result INTO DATA(segunda) INDEX 2.

    cl_abap_unit_assert=>assert_equals( exp = 'primeira' act = primeira ).
    cl_abap_unit_assert=>assert_equals( exp = 'segunda' act = segunda ).

  ENDMETHOD.

  METHOD to_binary.
    DATA input TYPE string.
    DATA: bin_data TYPE solix_tab,
          size     TYPE i.

    cut->to_binary( EXPORTING input = input
                     IMPORTING binary_tab = bin_data size = size ).
    cl_abap_unit_assert=>assert_equals( exp = 0  act = size ).

    input = 'TEXXXXXXXXXXXXXTTTOOOOOOO OKKKKKKKKKKKKKKKKK'.
    cut->to_binary( EXPORTING input = input
                    IMPORTING binary_tab = bin_data size = size ).
    cl_abap_unit_assert=>assert_differs( exp = 0  act = size ).
  ENDMETHOD.

  METHOD to_xstring.
    DATA input TYPE string.
    DATA: xstring TYPE xstring.

    xstring = cut->to_xstring( input ).
    cl_abap_unit_assert=>assert_initial( act = xstring ).

    input = 'TEXXXXXXXXXXXXXTTTOOOOOOO OKKKKKKKKKKKKKKKKK'.

    xstring = cut->to_xstring( input ).
    cl_abap_unit_assert=>assert_not_initial( act = xstring ).
  ENDMETHOD.

  METHOD replace_characters.
    DATA: input TYPE string.

    CONCATENATE `'` 'MG 111111' `'` INTO input.

    DATA(output) = cut->replace_characters( input = input  ).
    cl_abap_unit_assert=>assert_equals( act = output exp = '111111' ).
  ENDMETHOD.

ENDCLASS.
