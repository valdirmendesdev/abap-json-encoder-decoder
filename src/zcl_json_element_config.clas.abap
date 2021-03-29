CLASS zcl_json_element_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS: abap.

    DATA:
      abap_name           TYPE string,
      ext_name            TYPE string,
      use_conversion_exit TYPE abap_bool VALUE abap_true,
      required            TYPE abap_bool VALUE abap_false,
      type                TYPE REF TO cl_abap_typedescr,
      children            TYPE REF TO cl_object_map READ-ONLY.

    METHODS:

      constructor
        IMPORTING
          abap_name TYPE csequence OPTIONAL,

      add_child
        IMPORTING json_field_config TYPE REF TO zcl_json_element_config,

      get_child_by_abap_name
        IMPORTING abap_name     TYPE csequence
        RETURNING VALUE(result) TYPE REF TO zcl_json_element_config,

      is_complex_type
        RETURNING VALUE(result) TYPE abap_bool,

      get_element_by_path
        IMPORTING path          TYPE csequence
        RETURNING VALUE(result) TYPE REF TO zcl_json_element_config,

      get_child_by_external_name
        IMPORTING external_name TYPE csequence
        RETURNING VALUE(result) TYPE REF TO zcl_json_element_config.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_json_element_config IMPLEMENTATION.


  METHOD add_child.

    IF me->children IS NOT BOUND.
      CREATE OBJECT me->children.
    ENDIF.

    me->children->put(
      EXPORTING
        key      = json_field_config->abap_name
        value    = json_field_config
    ).

  ENDMETHOD.


  METHOD constructor.
    me->abap_name = abap_name.
  ENDMETHOD.


  METHOD get_child_by_abap_name.

    IF is_complex_type( ) EQ abap_false.
      RETURN.
    ENDIF.

    result ?= me->children->get( abap_name ).

  ENDMETHOD.


  METHOD get_child_by_external_name.

    DATA: iterator TYPE REF TO if_object_collection_iterator,
          element  TYPE REF TO zcl_json_element_config.

    iterator = me->children->get_values_iterator( ).
    WHILE iterator->has_next( ) EQ abap_true.
      element ?= iterator->get_next( ).

      IF element->ext_name EQ external_name.
        result = element.
        RETURN.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD get_element_by_path.

    DATA: item  TYPE REF TO zcl_json_element_config,
          names TYPE STANDARD TABLE OF string,
          name  TYPE string.

    item = me.

    SPLIT path AT '.' INTO TABLE names.

    LOOP AT names INTO name.

      TRANSLATE name TO UPPER CASE.

      IF item->abap_name EQ name.
        CONTINUE.
      ENDIF.

      IF item->is_complex_type( ) EQ abap_false.
        RETURN.
      ENDIF.

      item = item->get_child_by_abap_name( name ).

      IF item IS NOT BOUND.
        RETURN.
      ENDIF.
    ENDLOOP.

    result ?= item.

  ENDMETHOD.


  METHOD is_complex_type.

    IF children IS BOUND AND children->size( ) > 0.
      result = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
