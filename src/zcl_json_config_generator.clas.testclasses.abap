CLASS ltcl_json_config_generator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF third,
        name_3 TYPE makt-maktx,
      END OF third,

      BEGIN OF second,
        name_2 TYPE char20,
        third  TYPE third,
      END OF second,

      BEGIN OF main,
        name_1 TYPE string,
        second TYPE second,
      END OF main,

      BEGIN OF table_line,
        name TYPE string,
      END OF table_line,

      t_table TYPE STANDARD TABLE OF table_line WITH DEFAULT KEY,

      BEGIN OF struct_table,
        name    TYPE string,
        table_1 TYPE t_table,
      END OF struct_table.

    DATA: struct       TYPE main,
          table_struct TYPE struct_table,
          cut          TYPE REF TO zcl_json_config_generator.

    METHODS:
      setup,

      assert_instance_is_bound
        IMPORTING
          instance TYPE REF TO object,

      check_list_size
        IMPORTING
          list TYPE REF TO cl_object_map
          size TYPE i,

      check_contains_key
        IMPORTING
          list TYPE REF TO cl_object_map
          key  TYPE string,

      assert_equals
        IMPORTING
          exp TYPE any
          act TYPE any
          msg TYPE csequence OPTIONAL,

      assert_instances_is_not_equals
        IMPORTING
          instance_1 TYPE any
          instance_2 TYPE any
          msg        TYPE csequence OPTIONAL,

      assert_not_bound
        IMPORTING
          obj TYPE REF TO object.

    METHODS:
      process_element                   FOR TESTING,
      process_deep_structure            FOR TESTING,
      process_deep_structure_camel      FOR TESTING,
      process_table                     FOR TESTING,
      process_two_times_instance        FOR TESTING,
      require_all_fields                FOR TESTING.
ENDCLASS.


CLASS ltcl_json_config_generator IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.


  METHOD assert_instance_is_bound.

    cl_abap_unit_assert=>assert_bound(
      EXPORTING
        act = instance
        msg = 'Should returns a valid instance'
    ).

  ENDMETHOD.


  METHOD check_list_size.

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = list->size( )
        exp = size
        msg = |List should have exact number of itens { size } passed by parameter|
    ).

  ENDMETHOD.


  METHOD check_contains_key.

    cl_abap_unit_assert=>assert_true(
      EXPORTING
        act = list->contains_key( key )
        msg = |List should have the key: { key }|
    ).

  ENDMETHOD.

  METHOD assert_equals.
    cl_abap_unit_assert=>assert_equals( msg = msg exp = exp act = act ).
  ENDMETHOD.

  METHOD assert_not_bound.

    cl_abap_unit_assert=>assert_not_bound(
      EXPORTING
        act              = obj
*        msg              =
    ).

  ENDMETHOD.

  METHOD process_element.

    DATA: name TYPE string.

    DATA(json_field_config) = cut->generate_data_type_config(
                                EXPORTING
                                    name   = 'NAME'
                                    data   = name ).

    assert_instance_is_bound( json_field_config ).
    assert_equals( exp = 'NAME'
                   act = json_field_config->abap_name ).

  ENDMETHOD.

  METHOD process_deep_structure.

    DATA(field_config) = cut->generate_data_type_config(
      EXPORTING
        data   = struct
    ).

    DATA(field_cfg_aux) = field_config.

    assert_instance_is_bound( field_config ).
    check_list_size( list = field_config->children
                     size = 2 ).
    field_cfg_aux ?= field_config->children->get( 'NAME_1' ).
    assert_equals(
      EXPORTING
        exp = 'name1'
        act = field_cfg_aux->ext_name
    ).
    field_config ?= field_config->children->get( 'SECOND' ).
    assert_instance_is_bound( field_config ).
    assert_equals(
      EXPORTING
        exp = 'second'
        act = field_config->ext_name
    ).
    check_list_size( list = field_config->children
                     size = 2 ).
    check_contains_key( list = field_config->children
                        key = 'NAME_2' ).
    field_cfg_aux ?= field_config->children->get( 'NAME_2' ).
    assert_equals(
      EXPORTING
        exp = 'name2'
        act = field_cfg_aux->ext_name
    ).
    check_contains_key( list = field_config->children
                        key = 'THIRD' ).
    field_config ?= field_config->children->get( 'THIRD' ).
    assert_instance_is_bound( field_config ).
    assert_equals(
      EXPORTING
        exp = 'third'
        act = field_config->ext_name
    ).
    check_list_size( list = field_config->children
                     size = 1 ).
    check_contains_key( list = field_config->children
                        key = 'NAME_3' ).
    field_cfg_aux ?= field_config->children->get( 'NAME_3' ).
    assert_equals(
      EXPORTING
        exp = 'name3'
        act = field_cfg_aux->ext_name
    ).

  ENDMETHOD.

  METHOD process_table.

    DATA: struct TYPE struct_table.

    DATA(json_field_config) = cut->generate_data_type_config(
      EXPORTING
        data   = struct
    ).

    assert_instance_is_bound( json_field_config ).
    check_list_size( list = json_field_config->children
                     size = 2 ).
    check_contains_key( list = json_field_config->children
                        key = 'NAME' ).
    check_contains_key( list = json_field_config->children
                        key = 'TABLE_1' ).
    json_field_config ?= json_field_config->children->get( 'TABLE_1' ).
    assert_instance_is_bound( json_field_config ).
    check_list_size( list = json_field_config->children
                     size = 1 ).
    check_contains_key( list = json_field_config->children
                        key = 'TLINE' ).
    json_field_config ?= json_field_config->children->get( 'TLINE' ).
    assert_instance_is_bound( json_field_config ).
    check_list_size( list = json_field_config->children
                     size = 1 ).
    check_contains_key( list = json_field_config->children
                        key = 'NAME' ).

  ENDMETHOD.

  METHOD process_deep_structure_camel.

    "Switching off camel case
    cut->name_to_camel_case = abap_false.
    DATA(field_config) = cut->generate_data_type_config(
      EXPORTING
        data   = struct
    ).

    DATA(field_cfg_aux) = field_config.

    assert_instance_is_bound( field_config ).
    check_list_size( list = field_config->children
                     size = 2 ).
    field_cfg_aux ?= field_config->children->get( 'NAME_1' ).
    assert_equals(
      EXPORTING
        exp = 'NAME_1'
        act = field_cfg_aux->abap_name
    ).
    field_config ?= field_config->children->get( 'SECOND' ).
    assert_instance_is_bound( field_config ).
    assert_equals(
      EXPORTING
        exp = 'SECOND'
        act = field_config->abap_name
    ).
    check_list_size( list = field_config->children
                     size = 2 ).
    check_contains_key( list = field_config->children
                        key = 'NAME_2' ).
    field_cfg_aux ?= field_config->children->get( 'NAME_2' ).
    assert_equals(
      EXPORTING
        exp = 'NAME_2'
        act = field_cfg_aux->abap_name
    ).
    check_contains_key( list = field_config->children
                        key = 'THIRD' ).
    field_config ?= field_config->children->get( 'THIRD' ).
    assert_instance_is_bound( field_config ).
    assert_equals(
      EXPORTING
        exp = 'THIRD'
        act = field_config->abap_name
    ).
    check_list_size( list = field_config->children
                     size = 1 ).
    check_contains_key( list = field_config->children
                        key = 'NAME_3' ).
    field_cfg_aux ?= field_config->children->get( 'NAME_3' ).
    assert_equals(
      EXPORTING
        exp = 'NAME_3'
        act = field_cfg_aux->abap_name
    ).

  ENDMETHOD.

  METHOD process_two_times_instance.

    DATA(json_field_config) = cut->generate_data_type_config(
      EXPORTING
        data   = struct
    ).

    DATA(json_field) = cut->generate_data_type_config(
      EXPORTING
        data   = struct
    ).

    assert_instances_is_not_equals(
      EXPORTING
        instance_1 = json_field_config
        instance_2 = json_field
*        msg =
    ).

  ENDMETHOD.

  METHOD assert_instances_is_not_equals.

    IF instance_2 NE instance_1.
      RETURN.
    ENDIF.

    cl_abap_unit_assert=>fail( msg ).

  ENDMETHOD.

  METHOD require_all_fields.

    cut->require_all_fields = abap_true.
    DATA(element_config) = cut->generate_data_type_config(
      EXPORTING
        data   = table_struct
    ).

    assert_equals( exp = abap_true act = element_config->required ).

    DATA: nested_element_config TYPE REF TO zcl_json_element_config.
    nested_element_config ?= element_config->children->get( 'NAME' ).
    assert_equals( exp = abap_true act = nested_element_config->required ).
    element_config ?= element_config->children->get( 'TABLE_1' ).
    assert_equals( exp = abap_true act = element_config->required ).

    element_config ?= element_config->children->get( 'TLINE' ).
    assert_equals( exp = abap_true act = element_config->required ).

    element_config ?= element_config->children->get( 'NAME' ).
    assert_equals( exp = abap_true act = element_config->required ).

  ENDMETHOD.

ENDCLASS.
