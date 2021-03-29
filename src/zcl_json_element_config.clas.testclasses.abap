*&---------------------------------------------------------------------*
*& Include zcl_json_element_config_t99
*&---------------------------------------------------------------------*
CLASS ltcl_json_element_config DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA: cut TYPE REF TO zcl_json_element_config.

    METHODS:
      setup.

    METHODS:
      check_if_is_complex_type          FOR TESTING RAISING cx_static_check,
      no_exists_child_abap_name         FOR TESTING RAISING cx_static_check,
      get_child_by_abap_name            FOR TESTING RAISING cx_static_check,
      simple_element_by_path            FOR TESTING RAISING cx_static_check,
      simple_element_by_path_lower      FOR TESTING RAISING cx_static_check,
      simple_element_by_invalid_path    FOR TESTING RAISING cx_static_check,
      struct_element_by_path            FOR TESTING RAISING cx_static_check,
      deep_struct_element_by_path       FOR TESTING RAISING cx_static_check,
      non_existent_element_by_path      FOR TESTING RAISING cx_static_check,
      non_existent_element_in_struct    FOR TESTING RAISING cx_static_check,
      get_child_by_external_name        FOR TESTING RAISING cx_static_check.

ENDCLASS.



CLASS ltcl_json_element_config IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD get_child_by_abap_name.
    DATA(element_config) = NEW zcl_json_element_config( ).
    element_config->abap_name = 'NAME_1'.
    cut->add_child( element_config ).
    cl_abap_unit_assert=>assert_bound( act = cut->get_child_by_abap_name( 'NAME_1' ) ).
  ENDMETHOD.

  METHOD no_exists_child_abap_name.
    cl_abap_unit_assert=>assert_not_bound( act = cut->get_child_by_abap_name( 'NAME_1' ) ).
  ENDMETHOD.

  METHOD check_if_is_complex_type.
    cl_abap_unit_assert=>assert_equals(
        exp = abap_false
        act = cut->is_complex_type( )
    ).

    DATA(config) = NEW zcl_json_element_config( ).
    cut->add_child( config ).

    cl_abap_unit_assert=>assert_equals(
        exp = abap_true
        act = cut->is_complex_type( )
    ).
  ENDMETHOD.

  METHOD simple_element_by_path.
    cut->abap_name = 'NAME'.
    DATA(element) = cut->get_element_by_path( 'NAME' ).
    cl_abap_unit_assert=>assert_bound( act = element ).
  ENDMETHOD.

  METHOD simple_element_by_path_lower.
    cut->abap_name = 'NAME'.
    DATA(element) = cut->get_element_by_path( 'name' ).
    cl_abap_unit_assert=>assert_bound( act = element ).
  ENDMETHOD.

  METHOD simple_element_by_invalid_path.
    cut->abap_name = 'NAME'.
    DATA(element) = cut->get_element_by_path( 'NAME.' ).
    cl_abap_unit_assert=>assert_bound( act = element ).
  ENDMETHOD.

  METHOD struct_element_by_path.
    DATA(struct) = NEW zcl_json_element_config( 'SECOND' ).
    DATA(field) = NEW zcl_json_element_config( 'NAME' ).
    struct->add_child( field ).
    cut->add_child( struct ).
    DATA(element) = cut->get_element_by_path( 'SECOND.NAME' ).
    cl_abap_unit_assert=>assert_bound( act = element ).
    cl_abap_unit_assert=>assert_equals( exp = 'NAME' act = element->abap_name ).
  ENDMETHOD.

  METHOD deep_struct_element_by_path.
    DATA(struct) = NEW zcl_json_element_config( 'SECOND' ).
    DATA(struct1) = NEW zcl_json_element_config( 'THIRD' ).
    DATA(field) = NEW zcl_json_element_config( 'NAME' ).
    struct->add_child( struct1 ).
    struct1->add_child( field ).
    cut->add_child( struct ).
    DATA(element) = cut->get_element_by_path( 'SECOND.THIRD.NAME' ).
    cl_abap_unit_assert=>assert_bound( act = element ).
    cl_abap_unit_assert=>assert_equals( exp = 'NAME' act = element->abap_name ).
  ENDMETHOD.

  METHOD non_existent_element_by_path.
    cut->abap_name = 'NAME'.
    DATA(element) = cut->get_element_by_path( 'TEST' ).
    cl_abap_unit_assert=>assert_not_bound( act = element ).
  ENDMETHOD.

  METHOD non_existent_element_in_struct.
    DATA(struct) = NEW zcl_json_element_config( 'SECOND' ).
    DATA(field) = NEW zcl_json_element_config( 'NAME' ).
    struct->add_child( field ).
    cut->add_child( struct ).
    DATA(element) = cut->get_element_by_path( 'SECOND.TEST' ).
    cl_abap_unit_assert=>assert_not_bound( act = element ).
  ENDMETHOD.

  METHOD get_child_by_external_name.
    DATA(element_config) = NEW zcl_json_element_config( ).
    element_config->ext_name = 'name1'.
    cut->add_child( element_config ).
    element_config = NEW zcl_json_element_config( ).
    element_config->abap_name   = 'NAME_2'.
    element_config->ext_name    = 'name2'.
    cut->add_child( element_config ).
    cl_abap_unit_assert=>assert_bound( act = cut->get_child_by_external_name( 'name2' ) ).
  ENDMETHOD.

ENDCLASS.
