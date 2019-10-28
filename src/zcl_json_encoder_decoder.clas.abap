CLASS zcl_json_encoder_decoder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF options,
        use_conversion_exit TYPE abap_bool,
        camelcase           TYPE abap_bool,
        keep_empty_values   TYPE abap_bool,
      END OF options.

    METHODS:
      encode
        IMPORTING
          value         TYPE any
          options       TYPE zcl_json_encoder_decoder=>options
        RETURNING
          VALUE(result) TYPE string .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_boolean_types TYPE string VALUE '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL#\TYPE=BOOLEAN#\TYPE=BOOLE_D#\TYPE=XFELD'.

    METHODS:
      encode_value
        IMPORTING value   TYPE any
                  options TYPE zcl_json_encoder_decoder=>options
        CHANGING  result  TYPE string,
      encode_numeric
        IMPORTING
          value   TYPE any
          options TYPE zcl_json_encoder_decoder=>options
        CHANGING
          result  TYPE string,
      encode_time_to_iso
        IMPORTING
          value   TYPE any
          options TYPE zcl_json_encoder_decoder=>options
        CHANGING
          result  TYPE string,
      encode_date_sap_to_iso
        IMPORTING
          value   TYPE dats
          options TYPE zcl_json_encoder_decoder=>options
        CHANGING
          result  TYPE string,
      encode_time_sap_to_iso
        IMPORTING
          value   TYPE tims
          options TYPE zcl_json_encoder_decoder=>options
        CHANGING
          result  TYPE string ,
      type_is_boolean
        IMPORTING
          type          TYPE REF TO cl_abap_typedescr
        RETURNING
          VALUE(result) TYPE abap_bool,
      encode_abap_bool
        IMPORTING
          value   TYPE any
          options TYPE zcl_json_encoder_decoder=>options
        CHANGING
          result  TYPE string,
      encode_simple_value
        IMPORTING
          value   TYPE any
          options TYPE zcl_json_encoder_decoder=>options
          type    TYPE REF TO cl_abap_typedescr
        CHANGING
          result  TYPE string,
      run_conversion_exit_output
        IMPORTING
          value  TYPE any
          type   TYPE REF TO cl_abap_typedescr
        CHANGING
          result TYPE string,
      encode_struct
        IMPORTING
          value   TYPE any
          options TYPE zcl_json_encoder_decoder=>options
          type    TYPE REF TO cl_abap_typedescr
        CHANGING
          result  TYPE string,
      handle_case
        IMPORTING
          value         TYPE abap_compname
          options       TYPE zcl_json_encoder_decoder=>options
        RETURNING
          VALUE(result) TYPE string,
      camelcase
        IMPORTING
          value         TYPE string
        RETURNING
          VALUE(result) TYPE string,
      encode_table
        IMPORTING
          value   TYPE any
          options TYPE zcl_json_encoder_decoder=>options
          type    TYPE REF TO cl_abap_typedescr
        CHANGING
          result  TYPE string,
      value_is_valid
        IMPORTING value         TYPE any
                  options       TYPE zcl_json_encoder_decoder=>options
        RETURNING
                  VALUE(result) TYPE abap_bool.

ENDCLASS.



CLASS zcl_json_encoder_decoder IMPLEMENTATION.


  METHOD encode.
    me->encode_value(
        EXPORTING value = value
                  options = options
        CHANGING result = result
    ).
  ENDMETHOD.


  METHOD encode_value.

    DATA: reftype TYPE REF TO cl_abap_typedescr.

    reftype = cl_abap_typedescr=>describe_by_data( value ).

    CASE reftype->type_kind.
      WHEN  cl_abap_typedescr=>typekind_oref.

      WHEN  cl_abap_typedescr=>typekind_struct1 OR
            cl_abap_typedescr=>typekind_struct2.
        encode_struct(
          EXPORTING
            value = value
            options = options
            type    = reftype
          CHANGING
            result = result
        ).
      WHEN  cl_abap_typedescr=>typekind_table.

        encode_table(
          EXPORTING
            value = value
            options = options
            type    = reftype
          CHANGING
            result = result
        ).

      WHEN  cl_abap_typedescr=>typekind_date.
        encode_date_sap_to_iso(
          EXPORTING
            value = value
            options = options
          CHANGING
            result = result ).

      WHEN  cl_abap_typedescr=>typekind_time.
        encode_time_sap_to_iso(
          EXPORTING
            value = value
            options = options
          CHANGING
            result = result ).
      WHEN  cl_abap_typedescr=>typekind_packed      OR
            cl_abap_typedescr=>typekind_float       OR
            cl_abap_typedescr=>typekind_int         OR
            cl_abap_typedescr=>typekind_int1        OR
            cl_abap_typedescr=>typekind_int2        OR
            cl_abap_typedescr=>typekind_numeric     OR
            cl_abap_typedescr=>typekind_decfloat    OR
            cl_abap_typedescr=>typekind_decfloat16  OR
            cl_abap_typedescr=>typekind_decfloat34.

        IF reftype->get_relative_name( ) EQ 'TIMESTAMP'.
          encode_time_to_iso(
            EXPORTING
              value = value
              options = options
            CHANGING
              result = result ).
        ELSE.
          encode_numeric(
              EXPORTING
                  value = value
                  options = options
              CHANGING
                  result = result ).
        ENDIF.
      WHEN OTHERS.

        IF type_is_boolean( reftype ) EQ abap_true.
          encode_abap_bool(
            EXPORTING
                value = value
                options = options
            CHANGING
                result = result ).
        ELSE.
          encode_simple_value(
            EXPORTING
                value = value
                options = options
                type = reftype
            CHANGING
                result = result ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD encode_numeric.

    DATA: formatted_value TYPE string.

    IF value_is_valid(
       value   = value
       options = options ) EQ abap_false.
      RETURN.
    ENDIF.

    formatted_value = value.
    TRANSLATE formatted_value USING '- + '.
    CONDENSE formatted_value NO-GAPS.
    IF value < 0.
      CONCATENATE '-' formatted_value INTO formatted_value.
    ENDIF.

    CONCATENATE result
                formatted_value
           INTO result.

  ENDMETHOD.


  METHOD encode_time_to_iso.
    DATA: BEGIN OF ts_split,
            date TYPE datum,
            time TYPE uzeit,
          END OF ts_split,
          tsc(14)  TYPE n,
          date_iso TYPE string,
          time_iso TYPE string.

    IF value_is_valid(
       value   = value
       options = options ) EQ abap_false.
      RETURN.
    ENDIF.

    ts_split = tsc = value.
    encode_date_sap_to_iso(
      EXPORTING
        value  = ts_split-date
        options = options
      CHANGING
        result = date_iso
    ).
    encode_time_sap_to_iso(
      EXPORTING
        value  = ts_split-time
        options = options
      CHANGING
        result = time_iso
    ).
    REPLACE ALL OCCURRENCES OF '"' IN: date_iso WITH '',
                                       time_iso WITH ''.

    CONCATENATE '"' result date_iso 'T' time_iso '"' INTO result.
  ENDMETHOD.

  METHOD encode_date_sap_to_iso.

    IF value_is_valid(
       value   = value
       options = options ) EQ abap_false.
      RETURN.
    ENDIF.

    CONCATENATE '"' result VALUE(4) '-' value+4(2) '-' value+6(2) '"' INTO result.
  ENDMETHOD.

  METHOD encode_time_sap_to_iso.

    IF value_is_valid(
       value   = value
       options = options ) EQ abap_false.
      RETURN.
    ENDIF.

    CONCATENATE result '"' VALUE(2) ':' value+2(2) ':' value+4(2) '"' INTO result.
  ENDMETHOD.


  METHOD type_is_boolean.

    DATA: element_type TYPE REF TO cl_abap_elemdescr.

    IF type->kind NE cl_abap_typedescr=>kind_elem.
      RETURN.
    ENDIF.

    element_type ?= type.

    IF element_type->type_kind NE cl_abap_typedescr=>typekind_char OR
       element_type->output_length NE 1.
      result = abap_false.
      RETURN.
    ENDIF.

    IF c_boolean_types CS element_type->absolute_name.
      result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD encode_abap_bool.

    DATA: boolean_value TYPE string.

    IF value EQ abap_true.
      boolean_value = 'true'.
    ELSE.
      boolean_value = 'false'.
    ENDIF.
    CONCATENATE result boolean_value INTO result.

  ENDMETHOD.


  METHOD encode_simple_value.

    DATA: lv_value TYPE string.

    IF value_is_valid(
       value   = value
       options = options ) EQ abap_false.
      RETURN.
    ENDIF.

    lv_value = value.

    IF options-use_conversion_exit EQ abap_true.
      run_conversion_exit_output(
        EXPORTING
            value = value
            type  = type
        CHANGING result = lv_value ).
    ENDIF.

    CONCATENATE result '"' lv_value '"' INTO result.

  ENDMETHOD.


  METHOD run_conversion_exit_output.

    DATA: ddic_objects  TYPE dd_x031l_table,
          function_name TYPE string,
          cstr_value    TYPE c LENGTH 255.

    FIELD-SYMBOLS: <ddic> LIKE LINE OF ddic_objects.

    "// Call conversion exit function
    type->get_ddic_object(
      RECEIVING p_object = ddic_objects
      EXCEPTIONS OTHERS = 8
    ).

    IF sy-subrc NE 0 OR lines( ddic_objects ) EQ 0.
      result = value.
      RETURN.
    ENDIF.

    READ TABLE ddic_objects INDEX 1 ASSIGNING <ddic>.
    IF sy-subrc NE 0 AND <ddic>-convexit IS INITIAL.
      result = value.
      RETURN.
    ENDIF.
    cstr_value = value.
    CONCATENATE 'CONVERSION_EXIT_' <ddic>-convexit '_OUTPUT'
      INTO function_name.
    TRY.
        CALL FUNCTION function_name
          EXPORTING
            input  = cstr_value
          IMPORTING
            output = result
          EXCEPTIONS
            OTHERS = 8.
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD encode_struct.

    DATA: struct         TYPE REF TO cl_abap_structdescr,
          json_fieldname TYPE string,
          json_value     TYPE string,
          next           TYPE string.

    FIELD-SYMBOLS: <component> LIKE LINE OF struct->components,
                   <field>     TYPE any.

    "// Encode all class attributes
    struct ?= type.

    next = '{'.

    LOOP AT struct->components ASSIGNING <component>.
      ASSIGN COMPONENT <component>-name OF STRUCTURE value
        TO <field>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      json_fieldname = handle_case(
                        value   = <component>-name
                        options = options ).

      encode_value(
      EXPORTING
        value = <field>
        options = options
      CHANGING
        result = json_value
    ).

      IF json_value IS NOT INITIAL.
        CONCATENATE result next '"' json_fieldname '":' json_value INTO result.
        next = ','.
        FREE: json_value.
      ENDIF.

    ENDLOOP.

    IF next EQ '{'.
      CONCATENATE result '{}' INTO result.
    ELSE.
      CONCATENATE result '}' INTO result.
    ENDIF.

  ENDMETHOD.


  METHOD handle_case.

    result = value.
    TRANSLATE result TO LOWER CASE.

    IF options-camelcase EQ abap_true.
      result = camelcase( value = result ).
    ENDIF.

  ENDMETHOD.


  METHOD camelcase.

    DATA: tokens TYPE TABLE OF char128.
    FIELD-SYMBOLS: <token> LIKE LINE OF tokens.

    result = value.
    TRANSLATE result USING `/_:_~_`.
    SPLIT result AT `_` INTO TABLE tokens.
    DELETE tokens WHERE table_line IS INITIAL.
    LOOP AT tokens ASSIGNING <token> FROM 2.
      TRANSLATE <token>(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF tokens INTO result.

  ENDMETHOD.


  METHOD encode_table.

    DATA: table      TYPE REF TO cl_abap_tabledescr,
          lines_json TYPE TABLE OF string,
          line       TYPE string.

    FIELD-SYMBOLS: <table> TYPE ANY TABLE,
                   <line>  TYPE any.

    "// Encode all table lines
    table ?= type.

    ASSIGN value TO <table>.

    LOOP AT <table> ASSIGNING <line>.
      encode_value(
        EXPORTING
            value = <line>
            options = options
        CHANGING
            result = line ).

      IF line IS NOT INITIAL.
        APPEND line TO lines_json.
      ENDIF.

      FREE: line.

    ENDLOOP.

    CONCATENATE: result '[' INTO result,
                 LINES OF lines_json INTO line SEPARATED BY ',',
                 result line ']' INTO result.

  ENDMETHOD.


  METHOD value_is_valid.

    IF value IS INITIAL AND options-keep_empty_values EQ abap_false.
      RETURN.
    ENDIF.
    result = abap_true.
  ENDMETHOD.

ENDCLASS.
