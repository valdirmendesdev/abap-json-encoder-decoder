INTERFACE zif_datatype_iterator_ext
  PUBLIC .


  METHODS:
    before_table
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_tabledescr ,
    on_table_item
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_datadescr ,
    after_table
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_tabledescr ,
    before_struct
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_structdescr ,
    on_struct
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_typedescr ,
    after_struct
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_structdescr ,
    before_elem
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_elemdescr ,
    on_element
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_elemdescr ,
    after_elem
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_elemdescr ,
    before_ref
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_refdescr ,
    on_ref
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_refdescr ,
    after_ref
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_refdescr ,
    before_intf
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_intfdescr ,
    on_interface
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_intfdescr ,
    after_intf
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_intfdescr ,
    before_class
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_classdescr ,
    on_class
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_classdescr ,
    after_class
      IMPORTING name       TYPE csequence
                type_descr TYPE REF TO cl_abap_classdescr .
ENDINTERFACE.
