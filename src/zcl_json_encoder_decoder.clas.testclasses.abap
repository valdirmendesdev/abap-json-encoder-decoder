*"* use this source file for your ABAP unit test classes

CLASS lcl_obj_to_json DEFINITION.

  PUBLIC SECTION.

    DATA: public_name TYPE string.

    METHODS:
      get_name
        RETURNING VALUE(result) TYPE string,

      set_name
        IMPORTING name TYPE string,

      get_complex_name
        RETURNING VALUE(result) TYPE string,

      set_complex_name
        IMPORTING name TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA: name         TYPE string,
          complex_name TYPE string.

ENDCLASS.

CLASS lcl_obj_to_json IMPLEMENTATION.

  METHOD get_name.
    result = me->name.
  ENDMETHOD.

  METHOD set_name.
    me->name = name.
  ENDMETHOD.

  METHOD get_complex_name.
    result = me->complex_name.
  ENDMETHOD.

  METHOD set_complex_name.
    me->complex_name = name.
  ENDMETHOD.

ENDCLASS.

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
      empty_internal_table              FOR TESTING,
      obj_by_methods                    FOR TESTING,
      obj_by_attributes                 FOR TESTING.

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

  METHOD obj_by_methods.

    DATA: lo_obj_to_json TYPE REF TO lcl_obj_to_json,
          json_result    TYPE string.

    options-camelcase = abap_true.
    options-use_objs_methods = abap_true.

    CREATE OBJECT lo_obj_to_json.
    lo_obj_to_json->set_name( 'test' ).

    json_result = '{"name":"test"}'.
    check_scenario( options = options val = lo_obj_to_json exp  = json_result ).

  ENDMETHOD.

  METHOD obj_by_attributes.

    DATA: lo_obj_to_json TYPE REF TO lcl_obj_to_json,
          json_result    TYPE string.

    options-camelcase               = abap_true.
    options-use_public_attributes   = abap_true.

    CREATE OBJECT lo_obj_to_json.
    lo_obj_to_json->public_name = 'test'.

    json_result = '{"publicName":"test"}'.
    check_scenario( options = options val = lo_obj_to_json exp = json_result ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_json_decode DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF test_struct,
        valid     TYPE abap_bool,
        other     TYPE string,
        timestamp TYPE timestamp,
        time      TYPE sy-uzeit,
        date      TYPE sy-datum,
      END OF test_struct.

    DATA: o_cut   TYPE REF TO zcl_json_encoder_decoder,
          options TYPE zcl_json_encoder_decoder=>options.

    METHODS:
      setup,

      check_scenario
        IMPORTING
          json     TYPE clike
          expected TYPE any
        CHANGING
          actual   TYPE any,

      check_attribute
        IMPORTING
          json TYPE string
          exp  TYPE string.

    METHODS:
      struct                            FOR TESTING,
      table                             FOR TESTING,
      struct_camelcase_complex_names    FOR TESTING,
      struct_field_internal_table       FOR TESTING,
      struct_oo_internal_table          FOR TESTING,
      object_fill_public_attributes     FOR TESTING,
      object_fill_by_methods            FOR TESTING,
      obj_fill_complex_name             FOR TESTING,
      timestamp                         FOR TESTING,
      date                              FOR TESTING,
      time                              FOR TESTING,
      simple_attribute                  FOR TESTING,
      nested_named_struct               FOR TESTING.

ENDCLASS.

CLASS ltcl_json_decode IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT o_cut.
  ENDMETHOD.

  METHOD struct.

    DATA: lw_actual   TYPE ace_generic_range,
          lw_expected TYPE ace_generic_range.

    lw_expected-sign    = 'I'.
    lw_expected-option  = 'EQ'.
    lw_expected-low     = '0010'.

    check_scenario( EXPORTING
                        json = '{"sign":"I","option":"EQ","low":"0010","high":""}'
                        expected = lw_expected
                    CHANGING
                        actual   = lw_actual ).

  ENDMETHOD.


  METHOD check_scenario.

    o_cut->decode(
      EXPORTING
        json_string = json
        options      = options
      CHANGING
        result       = actual
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = actual
        exp = expected
    ).

  ENDMETHOD.

  METHOD table.

    DATA: lt_actual   TYPE ace_generic_range_t,
          lw_expected TYPE ace_generic_range,
          lt_expected TYPE ace_generic_range_t.

    lw_expected-sign    = 'I'.
    lw_expected-option  = 'EQ'.
    lw_expected-low     = '0010'.
    APPEND lw_expected TO lt_expected.
    lw_expected-low     = '0020'.
    APPEND lw_expected TO lt_expected.

    check_scenario( EXPORTING
                        json = '[{"sign":"I","option":"EQ","low":"0010","high":""},{"sign":"I","option":"EQ","low":"0020","high":""}]'
                        expected = lt_expected
                    CHANGING
                        actual   = lt_actual ).

  ENDMETHOD.

  METHOD struct_camelcase_complex_names.

    TYPES:
      BEGIN OF struct_complex_name,
        field_name TYPE string,
      END OF struct_complex_name.

    DATA: expected TYPE struct_complex_name,
          actual   TYPE struct_complex_name.

    options-camelcase   = abap_true.
    expected-field_name = 'Valdir'.

    check_scenario( EXPORTING
                        json = '{"fieldName":"Valdir"}'
                        expected = expected
                    CHANGING
                        actual   = actual ).
  ENDMETHOD.

  METHOD object_fill_public_attributes.

    DATA: lo_object   TYPE REF TO lcl_obj_to_json.

    CREATE OBJECT: lo_object.
    options-camelcase = options-use_public_attributes = abap_true.

    o_cut->decode(
      EXPORTING
        json_string = '{"publicName":"test"}'
        options      = options
      CHANGING
        result       = lo_object
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lo_object->public_name
        exp = 'test'
    ).

  ENDMETHOD.

  METHOD object_fill_by_methods.

    DATA: lo_object   TYPE REF TO lcl_obj_to_json.

    CREATE OBJECT: lo_object.
    options-camelcase = options-use_objs_methods = abap_true.

    o_cut->decode(
      EXPORTING
        json_string = '{"name":"test"}'
        options     = options
      CHANGING
        result       = lo_object
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lo_object->get_name( )
        exp = 'test'
    ).

  ENDMETHOD.

  METHOD obj_fill_complex_name.

    DATA: lo_object   TYPE REF TO lcl_obj_to_json.

    CREATE OBJECT: lo_object.
    options-camelcase = options-use_objs_methods = abap_true.

    o_cut->decode(
      EXPORTING
        json_string = '{"complexName":"test"}'
        options     = options
      CHANGING
        result       = lo_object
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lo_object->get_complex_name( )
        exp = 'test'
    ).

  ENDMETHOD.

  METHOD timestamp.

    DATA: expected TYPE test_struct,
          actual   TYPE test_struct.

    options-camelcase   = abap_true.
    expected-timestamp = '20191023145508'.

    check_scenario( EXPORTING
                        json = '{"timestamp":"2019-10-23T14:55:08"}'
                        expected = expected
                    CHANGING
                        actual   = actual ).

  ENDMETHOD.

  METHOD date.

    DATA: expected TYPE test_struct,
          actual   TYPE test_struct.

    options-camelcase   = abap_true.
    expected-date       = '20191023'.

    check_scenario( EXPORTING
                        json = '{"date":"2019-10-23"}'
                        expected = expected
                    CHANGING
                        actual   = actual ).
  ENDMETHOD.

  METHOD time.

    DATA: expected TYPE test_struct,
          actual   TYPE test_struct.

    options-camelcase   = abap_true.
    expected-time       = '145508'.

    check_scenario( EXPORTING
                        json = '{"time":"14:55:08"}'
                        expected = expected
                    CHANGING
                        actual   = actual ).
  ENDMETHOD.

  METHOD struct_field_internal_table.

    TYPES:
      BEGIN OF ty_it,
        name TYPE string,
      END OF ty_it,

      ty_it_t TYPE STANDARD TABLE OF ty_it WITH DEFAULT KEY,

      BEGIN OF struct_complex_name,
        field_name TYPE string,
        names      TYPE ty_it_t,
      END OF struct_complex_name.

    DATA: struct          TYPE struct_complex_name,
          actual          TYPE struct_complex_name,
          internal_struct TYPE ty_it.

    options-camelcase = abap_true.

    struct-field_name = 'test'.
    internal_struct-name = 'test'. APPEND internal_struct TO struct-names.
    check_scenario( EXPORTING
                    json = '{"fieldName":"test","names":[{"name":"test"}]}'
                    expected = struct
                CHANGING
                    actual   = actual ).

  ENDMETHOD.

  METHOD struct_oo_internal_table.

    TYPES:
      BEGIN OF struct_complex_name,
        field_name TYPE string,
        names      TYPE STANDARD TABLE OF REF TO lcl_obj_to_json WITH DEFAULT KEY,
      END OF struct_complex_name.

    DATA: obj    TYPE REF TO lcl_obj_to_json,
          actual TYPE struct_complex_name.

    options-camelcase        = abap_true.
    options-use_objs_methods = abap_true.

    o_cut->decode(
      EXPORTING
        json_string = '{"fieldName":"test","names":[{"name":"test"}]}'
        options     = options
      CHANGING
        result       = actual
    ).

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = actual-field_name
        exp = 'test'
    ).

    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = lines( actual-names )
            exp = 1
    ).

    obj = actual-names[ 1 ].

    cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = obj->get_name( )
            exp = 'test'
    ).
  ENDMETHOD.

  METHOD simple_attribute.
    check_attribute( json = '{"simple":"value"}' exp = 'value' ).
    check_attribute( json = '{"simple":true}' exp = 'X' ).
    check_attribute( json = '{"simple":false}' exp = '' ).
*    check_attribute( json = '{"simple":null}' exp = '' ).
*    check_attribute( json = '{"$simple":"value"}' exp = 'value' ).
  ENDMETHOD.


  METHOD check_attribute.

    TYPES:
      BEGIN OF struct,
        simple TYPE string,
      END OF struct.

    DATA: decoded TYPE struct.

    options-camelcase = abap_true.
    o_cut->decode(
      EXPORTING
        json_string = json
        options     = options
      CHANGING
        result       = decoded
    ).

    cl_abap_unit_assert=>assert_equals( msg = 'should returns filled struct'
                                        exp = decoded-simple
                                        act = exp ).

  ENDMETHOD.

  METHOD nested_named_struct.

    TYPES:
      BEGIN OF nested,
        name TYPE string,
      END OF nested,
      BEGIN OF main,
        nested TYPE nested,
      END OF main.

    DATA: decoded TYPE main.

    o_cut->decode(
      EXPORTING
        json_string = '{"nested":{"name":"test"}}'
        options     = options
      CHANGING
        result       = decoded
    ).

    cl_abap_unit_assert=>assert_equals( msg = 'Should returns filled struct'
                                        exp = 'test'
                                        act = decoded-nested-name ).

  ENDMETHOD.

ENDCLASS.

CLASS json_structure DEFINITION.

  PUBLIC SECTION.

    METHODS:
      add_element
        IMPORTING
          type  TYPE char1
          name  TYPE string OPTIONAL
          value TYPE string OPTIONAL,
      get_structure
        RETURNING
          VALUE(result) TYPE scanner=>json_element,
      level_up_element.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: actual_element TYPE REF TO data,
          elements       TYPE STANDARD TABLE OF REF TO data.
    METHODS create_element
      IMPORTING
        type          TYPE char1
        name          TYPE string
        value         TYPE string
      RETURNING
        VALUE(result) TYPE REF TO data.
    METHODS initialize_element
      IMPORTING
        type    TYPE char1
        name    TYPE string
        value   TYPE string
      CHANGING
        element TYPE scanner=>json_element.

ENDCLASS.

CLASS json_structure IMPLEMENTATION.

  METHOD add_element.

    IF me->actual_element IS NOT BOUND.
      me->actual_element = create_element( type  = type
                                           name  = name
                                           value = value ).
      APPEND me->actual_element TO me->elements.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS:
      <elements> TYPE scanner=>t_json_element,
      <element>  TYPE scanner=>json_element,
      <actual>   TYPE scanner=>json_element.

    ASSIGN:
     me->actual_element->* TO <element>,
     <element>-children->* TO <elements>.

    APPEND INITIAL LINE TO <elements> ASSIGNING <actual>.
    initialize_element(
      EXPORTING
        type    = type
        name    = name
        value   = value
      CHANGING
        element = <actual>
    ).
    GET REFERENCE OF <actual> INTO me->actual_element.
    APPEND me->actual_element TO me->elements.

  ENDMETHOD.


  METHOD create_element.
    FIELD-SYMBOLS: <json_element> TYPE scanner=>json_element.
    CREATE DATA: result TYPE scanner=>json_element.
    ASSIGN result->* TO <json_element>.
    initialize_element(
        EXPORTING
            type  = type
            name  = name
            value = value
        CHANGING
            element = <json_element> ).
  ENDMETHOD.


  METHOD get_structure.
    DATA: ref_first TYPE REF TO data.
    FIELD-SYMBOLS: <result> LIKE result.

    READ TABLE me->elements INTO ref_first INDEX 1.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    ASSIGN ref_first->* TO <result>.
    result = <result>.
  ENDMETHOD.


  METHOD initialize_element.
    CREATE DATA element-children TYPE scanner=>t_json_element.
    element-type     = type.
    element-name     = name.
    element-value    = value.
  ENDMETHOD.


  METHOD level_up_element.

    IF lines( me->elements ) <= 1.
      RETURN.
    ENDIF.

    READ TABLE me->elements INTO me->actual_element INDEX lines( me->elements )  - 1.
    DELETE me->elements INDEX lines( me->elements ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_scanner DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: cut            TYPE REF TO scanner,
          json_structure TYPE REF TO json_structure.

    METHODS:
      setup,

      check_json_element_tree
        IMPORTING expected      TYPE scanner=>json_element
                  actual        TYPE scanner=>json_element
        RETURNING VALUE(result) TYPE abap_bool.

    METHODS:
      is_valid                          FOR TESTING RAISING cx_static_check,
      object_json_tree_structure        FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_scanner IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD is_valid.

    TYPES:
      BEGIN OF valid_test,
        json TYPE string,
        ok   TYPE abap_bool,
      END OF valid_test.

    DATA: valid_tests TYPE STANDARD TABLE OF valid_test.

    valid_tests = VALUE #( ( json = 'foo' ok = abap_false )
                           ( json = '}{' ok = abap_false )
                           ( json = '{]' ok = abap_false )
                           ( json = '{}' ok = abap_true )
                           ( json = '{"foo":"bar"}' ok = abap_true )
                           ( json = '{"foo":"bar","bar":{"baz":["qux"]}}' ok = abap_true ) ).

    LOOP AT valid_tests ASSIGNING FIELD-SYMBOL(<test>).

      cl_abap_unit_assert=>assert_equals(
        EXPORTING
            act = cut->valid( <test>-json )
            exp = <test>-ok
            msg = |Json: { <test>-json } expected ok: { <test>-ok }| ).

    ENDLOOP.

  ENDMETHOD.

  METHOD object_json_tree_structure.

    TYPES:
      BEGIN OF test_instance,
        json     TYPE string,
        expected TYPE scanner=>json_element,
      END OF test_instance.

    DATA: test_instances TYPE STANDARD TABLE OF test_instance,
          json_element   TYPE scanner=>json_element.

    "Scenario: simple json
    json_structure = NEW #( ).
    json_structure->add_element( type = scanner=>json_element_type-object ).
    APPEND VALUE #( json = '{}' expected = json_structure->get_structure( ) ) TO test_instances.

    "Scenario: simple json with attributes
    json_structure = NEW #( ).
    json_structure->add_element( type = scanner=>json_element_type-object ).
    json_structure->add_element(
      EXPORTING
        type  = scanner=>json_element_type-attribute
        name  = 'atr'
        value = 'value'
    ).
    APPEND VALUE #( json = '{"atr":"value"}' expected = json_structure->get_structure( ) ) TO test_instances.

    json_structure = NEW #( ).
    json_structure->add_element( type = scanner=>json_element_type-object ).
    json_structure->add_element(
      EXPORTING
        type  = scanner=>json_element_type-attribute
        name  = 'atr'
        value = 'value'
    ).
    json_structure->level_up_element( ).
    json_structure->add_element(
      EXPORTING
        type  = scanner=>json_element_type-name
        name  = 'test'
*        value =
    ).
    json_structure->add_element(
      EXPORTING
        type  = scanner=>json_element_type-array
*        name  =
*        value =
    ).
    json_structure->add_element(
      EXPORTING
        type  = scanner=>json_element_type-object
*        name  =
*        value =
    ).
    json_structure->add_element(
      EXPORTING
        type  = scanner=>json_element_type-attribute
        name  = 'foo'
        value = 'bar'
    ).
    APPEND VALUE #( json = '{"atr":"value","test":[{"foo":"bar"}]}'
                    expected = json_structure->get_structure( ) ) TO test_instances.

    LOOP AT test_instances ASSIGNING FIELD-SYMBOL(<test>).

      cut->valid( json = <test>-json ).
      json_element = cut->get_json_element_tree( ).

      cl_abap_unit_assert=>assert_true(
        EXPORTING
          act = check_json_element_tree( expected = <test>-expected
                                         actual   = json_element )
          msg = |Scenario: { <test>-json }|
      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD check_json_element_tree.

    FIELD-SYMBOLS:
      <actual_children>   TYPE scanner=>t_json_element,
      <expected_children> TYPE scanner=>t_json_element,
      <expected>          TYPE scanner=>json_element,
      <actual>            TYPE scanner=>json_element.

    result = abap_true.

    IF expected-type NE actual-type.
      result = abap_false.
      RETURN.
    ENDIF.

    IF expected-name NE actual-name.
      result = abap_false.
      RETURN.
    ENDIF.

    IF expected-value NE actual-value.
      result = abap_false.
      RETURN.
    ENDIF.

    ASSIGN: expected-children->* TO <expected_children>,
            actual-children->* TO <actual_children>.

    IF lines( <expected_children> ) NE lines( <actual_children> ).
      result = abap_false.
      RETURN.
    ENDIF.

    LOOP AT <expected_children> ASSIGNING <expected>.

      READ TABLE <actual_children> ASSIGNING <actual> INDEX sy-tabix.

      result = check_json_element_tree( expected = <expected>
                                        actual   = <actual> ).

      IF result EQ abap_false.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
