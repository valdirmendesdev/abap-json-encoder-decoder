CLASS zcl_json_config_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS: abap.

    DATA: name_to_camel_case TYPE abap_bool,
          require_all_fields TYPE abap_bool.

    INTERFACES: zif_datatype_iterator_ext.

    METHODS:

      constructor
        IMPORTING
          name_to_camel      TYPE abap_bool DEFAULT abap_true
          require_all_fields TYPE abap_bool DEFAULT abap_false,

      generate_data_type_config
        IMPORTING data          TYPE any
                  name          TYPE csequence DEFAULT 'ROOT'
        RETURNING VALUE(result) TYPE REF TO zcl_json_element_config,

      get_field_config
        RETURNING VALUE(result) TYPE REF TO zcl_json_element_config.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: objects_stack       TYPE REF TO cl_object_collection,
          actual_field_config TYPE REF TO zcl_json_element_config,
          abap_type_iterator  TYPE REF TO zcl_datatype_iterator.

    METHODS:
      create_element
        IMPORTING name          TYPE csequence
                  type_descr    TYPE REF TO cl_abap_typedescr
        RETURNING VALUE(result) TYPE REF TO zcl_json_element_config,

      enqueue_object,

      dequeue_object
        RETURNING VALUE(result) TYPE REF TO zcl_json_element_config,

      add_element
        IMPORTING name          TYPE csequence
                  type_descr    TYPE REF TO cl_abap_typedescr
        RETURNING VALUE(result) TYPE REF TO zcl_json_element_config,

      add_complex_element
        IMPORTING
          name       TYPE csequence
          type_descr TYPE REF TO cl_abap_typedescr,

      generate_external_name
        IMPORTING name          TYPE csequence
        RETURNING VALUE(result) TYPE string.
ENDCLASS.



CLASS zcl_json_config_generator IMPLEMENTATION.


  METHOD zif_datatype_iterator_ext~after_class.

  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~after_elem.

  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~after_intf.

  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~after_ref.
  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~after_struct.
    me->dequeue_object( ).
  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~after_table.
    me->dequeue_object( ).
  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~before_class.

  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~before_elem.
  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~before_intf.

  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~before_ref.
  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~before_struct.
    me->add_complex_element( name = name
                             type_descr = type_descr ).
  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~before_table.
    me->add_complex_element( name = name
                             type_descr = type_descr ).
  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~on_class.

  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~on_element.

    DATA: type TYPE REF TO cl_abap_typedescr.

    type ?= type_descr.
    me->add_element( name = name
                     type_descr = type ).
  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~on_interface.

  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~on_ref.

  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~on_struct.

  ENDMETHOD.


  METHOD zif_datatype_iterator_ext~on_table_item.

  ENDMETHOD.


  METHOD add_complex_element.

    DATA: item TYPE REF TO zcl_json_element_config.
    me->enqueue_object( ).
    item = me->add_element( name = name
                            type_descr = type_descr ).
    actual_field_config = item.

  ENDMETHOD.


  METHOD add_element.

    result = create_element( name = name
                             type_descr = type_descr ).

    IF result EQ actual_field_config.
      RETURN.
    ENDIF.

    actual_field_config->add_child( result ).

  ENDMETHOD.


  METHOD constructor.
    me->name_to_camel_case = name_to_camel.
    me->require_all_fields = require_all_fields.
    CREATE OBJECT objects_stack.
    CREATE OBJECT abap_type_iterator
      EXPORTING
        datatype_processor = me.
  ENDMETHOD.


  METHOD create_element.

    CREATE OBJECT result
      EXPORTING
        abap_name = name.
    result->ext_name = generate_external_name( name ).
    result->type = type_descr.
    result->required = require_all_fields.

    IF actual_field_config IS NOT BOUND.
      actual_field_config = result.
    ENDIF.

  ENDMETHOD.


  METHOD dequeue_object.

    IF me->objects_stack->is_empty( ) EQ abap_true.
      RETURN.
    ENDIF.

    DATA: index TYPE i.
    index = me->objects_stack->size( ).
    actual_field_config ?= me->objects_stack->get( index ).
    me->objects_stack->remove( actual_field_config ).
    result = actual_field_config.

  ENDMETHOD.


  METHOD enqueue_object.
    IF me->actual_field_config IS BOUND.
      me->objects_stack->add( actual_field_config ).
    ENDIF.
  ENDMETHOD.


  METHOD generate_data_type_config.

    FREE: me->actual_field_config.

    me->abap_type_iterator->iterate(
      EXPORTING
        name = name
        data = data
    ).

    me->objects_stack->clear( ).

    DATA: type_descr TYPE REF TO cl_abap_typedescr.

    type_descr = cl_abap_typedescr=>describe_by_data( data ).
    IF type_descr->kind NE cl_abap_typedescr=>kind_elem.
      me->actual_field_config->required = abap_true.
    ENDIF.
    result = me->actual_field_config.
  ENDMETHOD.


  METHOD generate_external_name.

    DATA: utils TYPE REF TO zcl_string_utils.
    CREATE OBJECT utils.

    IF me->name_to_camel_case EQ abap_false.
      result = utils->to_lower_case( name ).
      RETURN.
    ENDIF.

    result = utils->to_lower_camel( name ).

  ENDMETHOD.


  METHOD get_field_config.
    result = me->actual_field_config.
  ENDMETHOD.
ENDCLASS.
