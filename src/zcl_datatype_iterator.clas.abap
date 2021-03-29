CLASS zcl_datatype_iterator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: datatype_processor TYPE REF TO zif_datatype_iterator_ext READ-ONLY.

    METHODS:
      constructor
        IMPORTING datatype_processor TYPE REF TO zif_datatype_iterator_ext,

      iterate
        IMPORTING name TYPE csequence DEFAULT 'ROOT'
                  data TYPE any.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      execute_abaptype_process
        IMPORTING name       TYPE csequence
                  data       TYPE any
                  type_descr TYPE REF TO cl_abap_typedescr,
      process_table
        IMPORTING name       TYPE csequence
                  data       TYPE any
                  type_descr TYPE REF TO cl_abap_typedescr,
      process_struct
        IMPORTING name       TYPE csequence
                  data       TYPE any
                  type_descr TYPE REF TO cl_abap_typedescr,
      process_elem
        IMPORTING name       TYPE csequence
                  data       TYPE any
                  type_descr TYPE REF TO cl_abap_typedescr,
      process_ref
        IMPORTING name       TYPE csequence
                  data       TYPE any
                  type_descr TYPE REF TO cl_abap_typedescr.

ENDCLASS.



CLASS zcl_datatype_iterator IMPLEMENTATION.


  METHOD constructor.
    me->datatype_processor = datatype_processor.
  ENDMETHOD.


  METHOD execute_abaptype_process.

    CASE type_descr->kind.
      WHEN cl_abap_typedescr=>kind_table.
        process_table( name       = name
                       data       = data
                       type_descr = type_descr ).
      WHEN cl_abap_typedescr=>kind_struct.
        process_struct( name       = name
                        data       = data
                        type_descr = type_descr ).
      WHEN cl_abap_typedescr=>kind_elem.
        process_elem( name       = name
                      data       = data
                      type_descr = type_descr ).
      WHEN cl_abap_typedescr=>kind_ref.
        process_ref( name       = name
                     data       = data
                     type_descr = type_descr ).
    ENDCASE.

  ENDMETHOD.


  METHOD iterate.

    DATA: type_descr TYPE REF TO cl_abap_typedescr.
    type_descr ?= cl_abap_typedescr=>describe_by_data( data ).

    execute_abaptype_process( name = name
                              data = data
                              type_descr = type_descr ).

  ENDMETHOD.


  METHOD process_elem.

    DATA: type       TYPE REF TO cl_abap_elemdescr.

    type ?= type_descr.

    me->datatype_processor->before_elem(
      EXPORTING
        name       = name
        type_descr = type
    ).

    me->datatype_processor->on_element(
      EXPORTING
        name       = name
        type_descr = type
    ).

    me->datatype_processor->after_elem(
      EXPORTING
        name       = name
        type_descr = type
    ).

  ENDMETHOD.


  METHOD process_ref.

    DATA: type TYPE REF TO cl_abap_refdescr.

    type ?= type_descr.

    me->datatype_processor->before_ref(
      EXPORTING
        name       = name
        type_descr = type
    ).

    me->execute_abaptype_process(
      EXPORTING
        name       = name
        data       = data
        type_descr = type
    ).

    me->datatype_processor->on_ref(
      EXPORTING
        name       = name
        type_descr = type
    ).

    me->datatype_processor->after_ref(
      EXPORTING
        name       = name
        type_descr = type
    ).

  ENDMETHOD.


  METHOD process_struct.

    DATA: type TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <comp>  LIKE LINE OF type->components,
                   <value> TYPE any.

    type ?= type_descr.

    me->datatype_processor->before_struct(
      EXPORTING
        name       = name
        type_descr = type
    ).

    DATA: elem TYPE REF TO cl_abap_typedescr.

    LOOP AT type->components ASSIGNING <comp>.

      ASSIGN COMPONENT <comp>-name OF STRUCTURE data TO <value>.

      elem = type->get_component_type( p_name = <comp>-name ).

      me->execute_abaptype_process(
        EXPORTING
          name       = <comp>-name
          data       = <value>
          type_descr = elem
      ).

      me->datatype_processor->on_struct(
        EXPORTING
          name       = <comp>-name
          type_descr = elem
      ).

    ENDLOOP.

    me->datatype_processor->after_struct(
      EXPORTING
        name       = name
        type_descr = type
    ).

  ENDMETHOD.


  METHOD process_table.

    DATA: type       TYPE REF TO cl_abap_tabledescr,
          table_line TYPE REF TO cl_abap_datadescr,
          struct_ref TYPE REF TO data.

    FIELD-SYMBOLS: <table>  TYPE table,
                   <struct> TYPE any.

    type ?= type_descr.

    me->datatype_processor->before_table(
      EXPORTING
        name       = name
        type_descr = type
    ).

    ASSIGN data TO <table>.
    CREATE DATA struct_ref LIKE LINE OF <table>.
    ASSIGN struct_ref->* TO <struct>.
    table_line = type->get_table_line_type( ).

    me->execute_abaptype_process(
      EXPORTING
        name       = 'TLINE'
        data       = <struct>
        type_descr = table_line
    ).

    LOOP AT <table> ASSIGNING <struct>.
      me->datatype_processor->on_table_item(
        EXPORTING
          name       = 'TDATA'
          type_descr = table_line
      ).
    ENDLOOP.


    me->datatype_processor->after_table(
      EXPORTING
        name       = name
        type_descr = type
    ).

  ENDMETHOD.
ENDCLASS.
