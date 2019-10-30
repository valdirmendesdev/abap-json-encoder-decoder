CLASS zcl_json_encoder_decoder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF options,
        use_conversion_exit   TYPE abap_bool,
        camelcase             TYPE abap_bool,
        keep_empty_values     TYPE abap_bool,
        use_objs_methods      TYPE abap_bool,
        use_public_attributes TYPE abap_bool,
      END OF options.

    METHODS:

      constructor,

      encode
        IMPORTING
          value         TYPE any
          options       TYPE zcl_json_encoder_decoder=>options
        RETURNING
          VALUE(result) TYPE string,

      decode
        IMPORTING json_string TYPE clike
        CHANGING  value       TYPE any.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_boolean_types TYPE string VALUE '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL#\TYPE=BOOLEAN#\TYPE=BOOLE_D#\TYPE=XFELD'.

    CONSTANTS:
      BEGIN OF json_element_type,
        object       TYPE char1 VALUE 'o',
        array        TYPE char1 VALUE 'r',
        attribute    TYPE char1 VALUE 'a',
        value_string TYPE char1 VALUE 's',
        value_true   TYPE char1 VALUE 't',
        value_false  TYPE char1 VALUE 'f',
      END OF json_element_type.

    TYPES:
      BEGIN OF json_element,
        type     TYPE char1,
        value    TYPE string,
        children TYPE REF TO data,
      END OF json_element,

      t_json_element TYPE STANDARD TABLE OF json_element.

    DATA: patterns TYPE STANDARD TABLE OF REF TO cl_abap_regex.

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
          value         TYPE clike
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
                  VALUE(result) TYPE abap_bool,
      encode_object
        IMPORTING
          value   TYPE any
          options TYPE zcl_json_encoder_decoder=>options
          type    TYPE REF TO cl_abap_typedescr
        CHANGING
          result  TYPE string,
      encode_object_by_methods
        IMPORTING
          value   TYPE any
          options TYPE zcl_json_encoder_decoder=>options
          type    TYPE REF TO cl_abap_typedescr
        CHANGING
          result  TYPE string,
      encode_obj_public_attributes
        IMPORTING
          value   TYPE any
          options TYPE zcl_json_encoder_decoder=>options
          type    TYPE REF TO cl_abap_typedescr
        CHANGING
          result  TYPE string,
      get_attribute_by_method
        IMPORTING
          value         TYPE clike
          options       TYPE zcl_json_encoder_decoder=>options
        RETURNING
          VALUE(result) TYPE string,
      decode_element
        IMPORTING
          json         TYPE string
          position     TYPE i
        EXPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          new_position TYPE i,
      decode_object
        IMPORTING
          json         TYPE string
          position     TYPE i
        EXPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          new_position TYPE i,
      create_element_json
        RETURNING
          VALUE(result) TYPE zcl_json_encoder_decoder=>json_element,
      decode_string_value
        IMPORTING
          json         TYPE string
          position     TYPE i
        EXPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          new_position TYPE i,
      transport_values
        EXPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
        CHANGING
          value        TYPE any,
      transfer_values_struct
        IMPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
        CHANGING
          value        TYPE any,
      transport_value_string
        IMPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
        CHANGING
          value        TYPE any.

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
        encode_object(
          EXPORTING
            value = value
            options = options
            type    = reftype
          CHANGING
            result = result
        ).
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


  METHOD encode_object.

    encode_object_by_methods(
          EXPORTING
            value = value
            options = options
            type    = type
          CHANGING
            result = result
        ).

    encode_obj_public_attributes(
      EXPORTING
        value = value
        options = options
        type    = type
      CHANGING
        result = result
    ).

  ENDMETHOD.


  METHOD encode_object_by_methods.

    IF options-use_objs_methods EQ abap_false. RETURN. ENDIF.

    DATA: reftype        TYPE REF TO cl_abap_refdescr,
          objtype        TYPE REF TO cl_abap_objectdescr,
          type_descr     TYPE REF TO cl_abap_typedescr,
          ref_value      TYPE REF TO data,
          lt_methods     TYPE abap_methdescr_tab,
          lw_param       TYPE abap_parmbind,
          lt_params      TYPE abap_parmbind_tab,
          o_obj          TYPE REF TO object,
          json_fieldname TYPE string,
          json_value     TYPE string,
          next           TYPE string.

    FIELD-SYMBOLS: <method_descr> TYPE abap_methdescr,
                   <param_descr>  TYPE abap_parmdescr,
                   <attribute>    TYPE any.

    o_obj = value.
    reftype ?= type.
    objtype ?= reftype->get_referenced_type( ).

    lt_methods = objtype->methods.
    DELETE lt_methods WHERE visibility NE cl_abap_objectdescr=>public.

    next = '{'.
    LOOP AT lt_methods ASSIGNING <method_descr>.

      IF lines( <method_descr>-parameters ) > 1.
        CONTINUE.
      ENDIF.

      READ TABLE <method_descr>-parameters
        ASSIGNING <param_descr> INDEX 1.

      CHECK <param_descr>-parm_kind EQ cl_abap_objectdescr=>returning.

      type_descr ?= objtype->get_method_parameter_type(
                 p_method_name       = <method_descr>-name
                 p_parameter_name    = <param_descr>-name
             ).

      CASE type_descr->kind.
        WHEN cl_abap_typedescr=>kind_elem.

          DATA: elem_descr TYPE REF TO cl_abap_elemdescr.

          elem_descr ?= type_descr.

          CREATE DATA ref_value TYPE HANDLE elem_descr.

        WHEN cl_abap_typedescr=>kind_struct.
        WHEN cl_abap_typedescr=>kind_table.

      ENDCASE.

      ASSIGN ref_value->* TO <attribute>.

      lw_param-name = <param_descr>-name.
      lw_param-kind = cl_abap_objectdescr=>receiving.
      lw_param-value = ref_value.
      INSERT lw_param INTO TABLE lt_params.

      CALL METHOD o_obj->(<method_descr>-name)
        PARAMETER-TABLE
        lt_params.

      encode_value(
        EXPORTING
          value   = <attribute>
          options = options
        CHANGING
          result  = json_value
      ).

      json_fieldname = get_attribute_by_method(
                        value   = <method_descr>-name
                        options = options ).

      IF json_value IS NOT INITIAL.
        CONCATENATE result next '"' json_fieldname '":' json_value INTO result.
        next = ','.
      ENDIF.

      FREE: lt_params,
            json_value.

    ENDLOOP.

    IF next EQ '{'.
      CONCATENATE result '{}' INTO result.
    ELSE.
      CONCATENATE result '}' INTO result.
    ENDIF.

  ENDMETHOD.


  METHOD encode_obj_public_attributes.

    IF options-use_public_attributes EQ abap_false. RETURN. ENDIF.

    DATA: ref            TYPE REF TO cl_abap_refdescr,
          obj            TYPE REF TO cl_abap_objectdescr,
          attribute_name TYPE string,
          json_fieldname TYPE string,
          json_value     TYPE string,
          next           TYPE string.

    FIELD-SYMBOLS: <attribute_descr> LIKE LINE OF obj->attributes,
                   <attribute>       TYPE any.

    "// Encode all obj attributes
    ref ?= type.
    obj ?= ref->get_referenced_type( ).

    next = '{'.
    LOOP AT obj->attributes ASSIGNING <attribute_descr>
        WHERE visibility = cl_abap_classdescr=>public.

      CONCATENATE 'value->' <attribute_descr>-name INTO attribute_name.

      ASSIGN (attribute_name) TO <attribute>.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      encode_value(
        EXPORTING
          value   = <attribute>
          options = options
        CHANGING
          result  = json_value
      ).

      json_fieldname = handle_case(
                        value   = <attribute_descr>-name
                        options = options ).

      IF json_value IS NOT INITIAL.
        CONCATENATE result next '"' json_fieldname '":' json_value INTO result.
        next = ','.
      ENDIF.

    ENDLOOP.

    IF next EQ '{'.
      CONCATENATE result '{}' INTO result.
    ELSE.
      CONCATENATE result '}' INTO result.
    ENDIF.

  ENDMETHOD.


  METHOD get_attribute_by_method.

    DATA: result_tab TYPE match_result_tab,
          lo_regex   TYPE REF TO cl_abap_regex.

    FIELD-SYMBOLS: <result_regex> LIKE LINE OF result_tab,
                   <submatch>     LIKE LINE OF <result_regex>-submatches.

    result = value.

    LOOP AT patterns INTO lo_regex.

      FIND FIRST OCCURRENCE OF REGEX lo_regex
        IN value
        RESULTS result_tab.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      CLEAR: result.

      LOOP AT result_tab ASSIGNING <result_regex>.

        LOOP AT <result_regex>-submatches ASSIGNING <submatch>.

          CONCATENATE result
                      value+<submatch>-offset(<submatch>-length)
                 INTO result.

        ENDLOOP.

      ENDLOOP.

      result = handle_case( value   = result
                            options = options ).

      RETURN.

    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.

    DATA: lo_regex   TYPE REF TO cl_abap_regex.

    CREATE OBJECT lo_regex
      EXPORTING
        pattern     = 'get_(\w*)'
        ignore_case = abap_true
*       simple_regex  = ABAP_FALSE
*       no_submatches = ABAP_FALSE
      .
    APPEND lo_regex TO me->patterns.

  ENDMETHOD.

  METHOD decode.

    DATA: condensed_string TYPE string,
          lw_decoded       TYPE json_element.

    condensed_string = json_string.
    CONDENSE condensed_string.

    decode_element(
        EXPORTING
            json     = condensed_string
            position = 0
        IMPORTING
            json_element = lw_decoded ).

    transport_values(
       IMPORTING
           json_element = lw_decoded
       CHANGING value = value
    ).

  ENDMETHOD.


  METHOD decode_element.

    DATA: lv_new_position TYPE i.

    FIELD-SYMBOLS: <children> TYPE t_json_element.

    json_element = create_element_json( ).
    ASSIGN json_element-children->* TO <children>.

    CASE json+position(1).
      WHEN '{'.
        lv_new_position = position + 1.
        decode_object(
          EXPORTING
            json         = json
            position     = lv_new_position
          IMPORTING
            json_element = json_element
            new_position = new_position
        ).
      WHEN '"'.
        decode_string_value(
          EXPORTING
            json         = json
            position     = position
          IMPORTING
            json_element = json_element
            new_position = new_position
        ).
    ENDCASE.

  ENDMETHOD.


  METHOD decode_object.

    DATA: lv_position             TYPE i,
          lv_property_name_length TYPE i,
          lv_property_name        TYPE string,
          lw_json_element         TYPE json_element.

    FIELD-SYMBOLS: <children>       TYPE t_json_element,
                   <child_children> TYPE t_json_element,
                   <element>        TYPE json_element.

    json_element = create_element_json( ).
    ASSIGN json_element-children->* TO <children>.
    json_element-type = zcl_json_encoder_decoder=>json_element_type-object.

    lv_position = position.

    DO.

      CASE json+lv_position(1).
        WHEN '}'.
          new_position = lv_position + 1.
          EXIT.
        WHEN ','.
          lv_position = lv_position + 1.
      ENDCASE.

      FIND REGEX '\A\s*"([^:]*)"\s*:' IN SECTION OFFSET lv_position OF json
                        MATCH OFFSET lv_position MATCH LENGTH lv_property_name_length
                        SUBMATCHES lv_property_name.

      IF sy-subrc NE 0.

      ENDIF.

      lv_position = lv_position + lv_property_name_length.

      lw_json_element = create_element_json( ).
      lw_json_element-type = zcl_json_encoder_decoder=>json_element_type-attribute.
      lw_json_element-value = lv_property_name.

      APPEND lw_json_element TO <children> ASSIGNING <element>.

      decode_element(
          EXPORTING
            json         = json
            position     = lv_position
          IMPORTING
            json_element = lw_json_element
            new_position = lv_position
        ).

      ASSIGN <element>-children->* TO <child_children>.
      APPEND lw_json_element TO <child_children>.
      new_position = lv_position.

    ENDDO.

  ENDMETHOD.


  METHOD create_element_json.
    CREATE DATA result-children TYPE t_json_element.
  ENDMETHOD.


  METHOD decode_string_value.

    DATA: lv_value_lenght TYPE i.

    json_element-type = zcl_json_encoder_decoder=>json_element_type-value_string.

    FIND REGEX '"([^"]*)"' IN SECTION OFFSET position OF json
        MATCH OFFSET new_position MATCH LENGTH lv_value_lenght
        SUBMATCHES json_element-value.

    new_position = position + lv_value_lenght.
  ENDMETHOD.


  METHOD transport_values.

    DATA: reftype TYPE REF TO cl_abap_typedescr.

    reftype = cl_abap_typedescr=>describe_by_data( value ).

    CASE reftype->kind.
      WHEN cl_abap_typedescr=>kind_struct.
        transfer_values_struct(
          EXPORTING
            json_element = json_element
          CHANGING value = value
        ).

    ENDCASE.

  ENDMETHOD.


  METHOD transfer_values_struct.

    IF json_element-type NE json_element_type-object. RETURN. ENDIF.

    FIELD-SYMBOLS: <children> TYPE t_json_element,
                   <child>    TYPE json_element,
                   <field>    TYPE any.

    ASSIGN json_element-children->* TO <children>.

    LOOP AT <children> ASSIGNING <child>.

      CASE <child>-type.
        WHEN json_element_type-attribute.

          ASSIGN COMPONENT <child>-value OF STRUCTURE value TO <field>.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.

          transport_value_string(
            EXPORTING json_element = <child>
            CHANGING value = <field>
           ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD transport_value_string.

    FIELD-SYMBOLS: <children> TYPE t_json_element,
                   <child>    TYPE json_element.

    ASSIGN json_element-children->* TO <children>.

    READ TABLE <children> ASSIGNING <child> INDEX 1.

    value = <child>-value.

  ENDMETHOD.

ENDCLASS.
