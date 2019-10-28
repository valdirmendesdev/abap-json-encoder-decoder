*"* use this source file for your ABAP unit test classes
CLASS ltcl_json_encode DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: o_cut   TYPE REF TO zcl_json_encoder_decoder,
          options TYPE zcl_json_encoder_decoder=>options.

    METHODS:
      setup,

      check_scenario
        IMPORTING
          val     TYPE any
          exp     TYPE string
          options TYPE zcl_json_encoder_decoder=>options OPTIONAL,

      get_type_title
        IMPORTING
          val           TYPE any
        RETURNING
          VALUE(result) TYPE string.

    METHODS:
      simple_values                     FOR TESTING,
      boolean_values                    FOR TESTING,
      struct_simple_names               FOR TESTING,
      struct_complex_names_camelcase    FOR TESTING,
      struct_complex_names              FOR TESTING,
      struct_keeping_empty_values       FOR TESTING,
      struct_empty                      FOR TESTING,
      internal_table                    FOR TESTING,
      empty_internal_table              FOR TESTING.

ENDCLASS.


CLASS ltcl_json_encode IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT o_cut.
    FREE: options.
  ENDMETHOD.

  METHOD simple_values.

    DATA: value_string          TYPE string,
          value_char            TYPE char1,
          value_int             TYPE i,
          value_date            TYPE sy-datum,
          value_time            TYPE sy-uzeit,
          value_timestamp       TYPE timestamp,
          value_float           TYPE decfloat16,
          value_negative_float  TYPE decfloat16,
          value_conversion_exit TYPE matnr.

    value_string            = 'test'.
    value_char              = 'a'.
    value_int               = 10.
    value_date              = '20191023'.
    value_time              = '112200'.
    value_timestamp         = '20191023145508'.
    value_float             = '10.0203'.
    value_negative_float    = '10.0203-'.
    value_conversion_exit   = '000000000000000018'.

    options-use_conversion_exit = abap_true.

    "Without name
    check_scenario( options = options val = value_string          exp = '"test"' ).
    check_scenario( options = options val = value_char            exp = '"a"' ).
    check_scenario( options = options val = value_int             exp = '10' ).
    check_scenario( options = options val = value_float           exp = '10.0203' ).
    check_scenario( options = options val = value_negative_float  exp = '-10.0203' ).
    check_scenario( options = options val = value_conversion_exit exp = '"18"' ).
    check_scenario( options = options val = value_date            exp = '"2019-10-23"' ).
    check_scenario( options = options val = value_time            exp = '"11:22:00"' ).
    check_scenario( options = options val = value_timestamp       exp = '"2019-10-23T14:55:08"' ).

    "Empty values
    FREE: value_string,
          value_int,
          value_conversion_exit,
          value_date,
          value_time,
          value_timestamp.
    check_scenario( options = options val = value_string          exp = '' ).
    check_scenario( options = options val = value_int             exp = '' ).
    check_scenario( options = options val = value_conversion_exit exp = '' ).
    check_scenario( options = options val = value_date            exp = '' ).
    check_scenario( options = options val = value_time            exp = '' ).
    check_scenario( options = options val = value_timestamp       exp = '' ).

  ENDMETHOD.


  METHOD check_scenario.

    DATA: result     TYPE string,
          type_title TYPE string,
          error      TYPE string.

    result = o_cut->encode(
      EXPORTING
        value = val
        options = options
    ).

    type_title = get_type_title( val ).

    CONCATENATE 'Error in encoding of type '
                type_title
           INTO error SEPARATED BY space.

    cl_abap_unit_assert=>assert_equals( msg = error
                                        exp = exp
                                        act = result ).

  ENDMETHOD.

  METHOD get_type_title.

    DATA: type TYPE REF TO cl_abap_typedescr.
    type   = cl_abap_typedescr=>describe_by_data( val ).
    result = type->get_relative_name( ).

  ENDMETHOD.

  METHOD boolean_values.

    DATA: value_bool    TYPE abap_bool,
          value_boolean TYPE boolean,
          value_bool_d  TYPE boole_d,
          value_xfeld   TYPE xfeld.

    "Check boolean false
    check_scenario( options = options val = value_bool    exp = 'false' ).
    check_scenario( options = options val = value_boolean exp = 'false' ).
    check_scenario( options = options val = value_bool_d  exp = 'false' ).
    check_scenario( options = options val = value_xfeld   exp = 'false' ).

    "Check boolean true
    value_bool = value_boolean = value_bool_d = value_xfeld = 'X'.
    check_scenario( options = options val = value_bool    exp = 'true' ).
    check_scenario( options = options val = value_boolean exp = 'true' ).
    check_scenario( options = options val = value_bool_d  exp = 'true' ).
    check_scenario( options = options val = value_xfeld   exp = 'true' ).

  ENDMETHOD.

  METHOD struct_simple_names.

    DATA: lw_range    TYPE ace_generic_range,
          json_result TYPE string.

    options-keep_empty_values   = abap_true.

    lw_range-sign   = 'I'.
    lw_range-option = 'EQ'.
    lw_range-low    = '0010'.
    json_result = '{"sign":"I","option":"EQ","low":"0010","high":""}'.
    check_scenario( options = options val = lw_range exp  = json_result ).

  ENDMETHOD.

  METHOD struct_complex_names_camelcase.

    TYPES:
      BEGIN OF struct_complex_name,
        field_name TYPE string,
      END OF struct_complex_name.

    DATA: struct      TYPE struct_complex_name,
          json_result TYPE string.

    options-camelcase = abap_true.
    options-use_conversion_exit = abap_true.

    struct-field_name = 'test'.
    json_result = '{"fieldName":"test"}'.
    check_scenario( options = options val = struct exp  = json_result ).

  ENDMETHOD.

  METHOD struct_complex_names.

    TYPES:
      BEGIN OF struct_complex_name,
        field_name TYPE string,
      END OF struct_complex_name.

    DATA: struct      TYPE struct_complex_name,
          json_result TYPE string.

*    options-camelcase = abap_true.
    options-use_conversion_exit = abap_true.

    struct-field_name = 'test'.
    json_result = '{"field_name":"test"}'.
    check_scenario( options = options val = struct exp  = json_result ).

  ENDMETHOD.

  METHOD struct_keeping_empty_values.

    DATA: lw_range    TYPE ace_generic_range,
          json_result TYPE string.

    options-keep_empty_values = abap_true.
    json_result = '{"sign":"","option":"","low":"","high":""}'.
    check_scenario( options = options val = lw_range exp  = json_result ).

  ENDMETHOD.

  METHOD struct_empty.

    DATA: lw_range    TYPE ace_generic_range,
          json_result TYPE string.

    options-keep_empty_values = abap_false.
    json_result = '{}'.
    check_scenario( options = options val = lw_range exp  = json_result ).

  ENDMETHOD.

  METHOD internal_table.

    DATA: lw_range    TYPE ace_generic_range,
          lt_range    TYPE ace_generic_range_t,
          json_result TYPE string.

    options-camelcase = abap_true.
    options-use_conversion_exit = abap_true.
    options-keep_empty_values   = abap_true.

    lw_range-sign   = 'I'.
    lw_range-option = 'EQ'.
    lw_range-low    = '0010'.
    APPEND lw_range TO lt_range.
    lw_range-low    = '0020'.
    APPEND lw_range TO lt_range.

    json_result = '[{"sign":"I","option":"EQ","low":"0010","high":""},{"sign":"I","option":"EQ","low":"0020","high":""}]'.
    check_scenario(
        options = options
        val     = lt_range
        exp     = json_result
    ).

  ENDMETHOD.

  METHOD empty_internal_table.

    DATA: lw_range    TYPE ace_generic_range_t,
          json_result TYPE string.

    options-keep_empty_values = abap_false.
    json_result = '[]'.
    check_scenario( options = options val = lw_range exp  = json_result ).
  ENDMETHOD.

ENDCLASS.
