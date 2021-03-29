CLASS zcl_number_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS: abap.

    METHODS:

      constructor.

    METHODS:
      is_numeric
        IMPORTING value         TYPE any
        RETURNING VALUE(result) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: numeric_types TYPE SORTED TABLE OF abap_typekind WITH UNIQUE KEY table_line.

ENDCLASS.



CLASS zcl_number_utils IMPLEMENTATION.


  METHOD constructor.

    DATA: numeric_kind_names TYPE STANDARD TABLE OF string,
          dynamic_name       TYPE string.

    FIELD-SYMBOLS: <typekind>   TYPE string,
                   <kind_value> TYPE c.

    APPEND: 'TYPEKIND_DECFLOAT'     TO numeric_kind_names,
            'TYPEKIND_DECFLOAT16'   TO numeric_kind_names,
            'TYPEKIND_DECFLOAT34'   TO numeric_kind_names,
            'TYPEKIND_FLOAT'        TO numeric_kind_names,
            'TYPEKIND_INT'          TO numeric_kind_names,
            'TYPEKIND_INT1'         TO numeric_kind_names,
            'TYPEKIND_INT2'         TO numeric_kind_names,
            'TYPEKIND_INT8'         TO numeric_kind_names,
            'TYPEKIND_NUM'          TO numeric_kind_names,
            'TYPEKIND_NUMERIC'      TO numeric_kind_names,
            'TYPEKIND_PACKED'       TO numeric_kind_names.

    LOOP AT numeric_kind_names ASSIGNING <typekind>.
      CONCATENATE 'CL_ABAP_TYPEDESCR=>' <typekind>
             INTO dynamic_name.

      ASSIGN (dynamic_name) TO <kind_value>.

      IF <kind_value> IS ASSIGNED.
        INSERT <kind_value> INTO TABLE me->numeric_types.
      ENDIF.
      UNASSIGN: <kind_value>.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_numeric.

    DATA: type_descr TYPE REF TO cl_abap_typedescr.
    type_descr = cl_abap_typedescr=>describe_by_data( value ).

    READ TABLE me->numeric_types TRANSPORTING NO FIELDS WITH TABLE KEY table_line = type_descr->type_kind.

    IF sy-subrc EQ 0.
      result = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
