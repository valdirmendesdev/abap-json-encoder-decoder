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
                  options     TYPE zcl_json_encoder_decoder=>options
        CHANGING  value       TYPE any.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_boolean_types TYPE string VALUE '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL#\TYPE=BOOLEAN#\TYPE=BOOLE_D#\TYPE=XFELD'.

    CONSTANTS:
      BEGIN OF json_element_type,
        object        TYPE char1 VALUE 'o',
        array         TYPE char1 VALUE 'r',
        attribute     TYPE char1 VALUE 'a',
        value_string  TYPE char1 VALUE 's',
        value_boolean TYPE char1 VALUE 'b',
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
      handle_encode_case
        IMPORTING
          value         TYPE clike
          options       TYPE zcl_json_encoder_decoder=>options
        RETURNING
          VALUE(result) TYPE string,
      encode_camelcase
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
      transfer_values
        IMPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          options      TYPE zcl_json_encoder_decoder=>options
        CHANGING
          value        TYPE any,
      transfer_values_struct
        IMPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          options      TYPE zcl_json_encoder_decoder=>options
        CHANGING
          value        TYPE any,
      transfer_value_string
        IMPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          options      TYPE zcl_json_encoder_decoder=>options
          reftype      TYPE REF TO cl_abap_typedescr
        CHANGING
          value        TYPE any,
      decode_array
        IMPORTING
          json         TYPE string
          position     TYPE i
        EXPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          new_position TYPE i,
      transfer_values_table
        IMPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          options      TYPE zcl_json_encoder_decoder=>options
          reftype      TYPE REF TO cl_abap_typedescr
        CHANGING
          value        TYPE any,
      handle_decode_case
        IMPORTING
          value         TYPE clike
          options       TYPE zcl_json_encoder_decoder=>options
        RETURNING
          VALUE(result) TYPE string,
      decode_camelcase
        IMPORTING
          value         TYPE clike
        RETURNING
          VALUE(result) TYPE string,
      transfer_values_object
        IMPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          options      TYPE zcl_json_encoder_decoder=>options
          reftype      TYPE REF TO cl_abap_typedescr
        CHANGING
          value        TYPE any,
      transfer_to_public_attribute
        IMPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          options      TYPE zcl_json_encoder_decoder=>options
        CHANGING
          value        TYPE any,
      transfer_value_by_method
        IMPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          options      TYPE zcl_json_encoder_decoder=>options
          reftype      TYPE REF TO cl_abap_typedescr
        CHANGING
          value        TYPE any,
      decode_boolean_value
        IMPORTING
          json         TYPE string
          position     TYPE i
        EXPORTING
          json_element TYPE zcl_json_encoder_decoder=>json_element
          new_position TYPE i,
      timestamp_iso_to_sap
        IMPORTING
          timestamp     TYPE string
        RETURNING
          VALUE(result) TYPE timestamp,
      date_iso_to_sap
        IMPORTING
          date_iso      TYPE string
        RETURNING
          VALUE(result) TYPE string,
      time_iso_to_sap
        IMPORTING
          time_iso      TYPE string
        RETURNING
          VALUE(result) TYPE string.

ENDCLASS.



CLASS zcl_json_encoder_decoder IMPLEMENTATION.


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


  METHOD create_element_json.
    CREATE DATA result-children TYPE t_json_element.
  ENDMETHOD.


  METHOD decode.

    DATA: condensed_string TYPE string,
          lw_decoded       TYPE json_element.

    condensed_string = json_string.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN condensed_string WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN condensed_string WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN condensed_string WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed IN condensed_string WITH ''.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>vertical_tab IN condensed_string WITH ''.
    REPLACE ALL OCCURRENCES OF REGEX '(\\t|\\r|\\n|\\f)' IN condensed_string WITH ''.
    REPLACE ALL OCCURRENCES OF REGEX '\\"' IN condensed_string WITH '"'.
    CONDENSE condensed_string NO-GAPS.

    decode_element(
        EXPORTING
            json     = condensed_string
            position = 0
        IMPORTING
            json_element = lw_decoded ).

    transfer_values(
       EXPORTING
           json_element = lw_decoded
           options      = options
       CHANGING value = value
    ).

  ENDMETHOD.


  METHOD decode_array.

    DATA: lv_position     TYPE i,
          lw_json_element TYPE json_element.

    FIELD-SYMBOLS: <children>       TYPE t_json_element.

    json_element = create_element_json( ).
    ASSIGN json_element-children->* TO <children>.
    json_element-type = zcl_json_encoder_decoder=>json_element_type-array.

    lv_position = position.

    DO.

      CASE json+lv_position(1).
        WHEN ']'.
          new_position = lv_position + 1.
          EXIT.
        WHEN ','.
          lv_position = lv_position + 1.
      ENDCASE.

      decode_element(
          EXPORTING
            json         = json
            position     = lv_position
          IMPORTING
            json_element = lw_json_element
            new_position = lv_position
        ).

      APPEND lw_json_element TO <children>.
      new_position = lv_position.

    ENDDO.

  ENDMETHOD.


  METHOD decode_boolean_value.

    DATA: lv_value_lenght TYPE i.

    json_element-type = zcl_json_encoder_decoder=>json_element_type-value_boolean.

    FIND REGEX '(false|true)' IN SECTION OFFSET position OF json
        MATCH OFFSET new_position MATCH LENGTH lv_value_lenght
        SUBMATCHES json_element-value.

    new_position = position + lv_value_lenght.

  ENDMETHOD.


  METHOD decode_camelcase.
    result = value.
    REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN result WITH `$1_$2`.
  ENDMETHOD.


  METHOD decode_element.

    DATA: lv_new_position TYPE i.

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
      WHEN '['.
        lv_new_position = position + 1.
        decode_array(
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
      WHEN 'f' OR 't'.
        decode_boolean_value(
          EXPORTING
            json         = json
            position     = position
          IMPORTING
            json_element = json_element
            new_position = new_position
        ).
      WHEN 'n'.
        new_position = position + 4.
        FREE: json_element-value.
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


  METHOD decode_string_value.

    DATA: lv_value_lenght TYPE i.

    json_element-type = zcl_json_encoder_decoder=>json_element_type-value_string.

    FIND REGEX '"([^"]*)"' IN SECTION OFFSET position OF json
        MATCH OFFSET new_position MATCH LENGTH lv_value_lenght
        SUBMATCHES json_element-value.

    new_position = position + lv_value_lenght.
  ENDMETHOD.


  METHOD encode.
    me->encode_value(
        EXPORTING value = value
                  options = options
        CHANGING result = result
    ).
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


  METHOD encode_camelcase.

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


  METHOD encode_date_sap_to_iso.

    IF value_is_valid(
       value   = value
       options = options ) EQ abap_false.
      RETURN.
    ENDIF.

    CONCATENATE '"' result VALUE(4) '-' value+4(2) '-' value+6(2) '"' INTO result.
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

      json_fieldname = handle_encode_case(
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

      json_fieldname = handle_encode_case(
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


  METHOD encode_time_sap_to_iso.

    IF value_is_valid(
       value   = value
       options = options ) EQ abap_false.
      RETURN.
    ENDIF.

    CONCATENATE result '"' VALUE(2) ':' value+2(2) ':' value+4(2) '"' INTO result.
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

      result = handle_encode_case( value   = result
                            options = options ).

      RETURN.

    ENDLOOP.

  ENDMETHOD.


  METHOD handle_decode_case.

    result = value.

    IF options-camelcase EQ abap_true.
      result = decode_camelcase( value = result ).
    ENDIF.

    TRANSLATE result TO UPPER CASE.

  ENDMETHOD.


  METHOD handle_encode_case.

    result = value.
    TRANSLATE result TO LOWER CASE.

    IF options-camelcase EQ abap_true.
      result = encode_camelcase( value = result ).
    ENDIF.

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


  METHOD transfer_to_public_attribute.

    IF options-use_public_attributes NE abap_true. RETURN. ENDIF.

    DATA: attribute_name TYPE string.

    FIELD-SYMBOLS: <attribute> TYPE any.

    attribute_name = handle_decode_case(
                   value   = json_element-value
                   options = options
               ).

    CONCATENATE 'value->' attribute_name INTO attribute_name.
    ASSIGN (attribute_name) TO <attribute>.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    transfer_values(
      EXPORTING
        json_element = json_element
        options      = options
      CHANGING
        value        = <attribute>
    ).

  ENDMETHOD.


  METHOD transfer_values.

    DATA: reftype TYPE REF TO cl_abap_typedescr.

    reftype = cl_abap_typedescr=>describe_by_data( value ).

    CASE reftype->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        transfer_value_string(
          EXPORTING
            json_element = json_element
            options      = options
            reftype      = reftype
          CHANGING
            value        = value
        ).
      WHEN cl_abap_typedescr=>kind_struct.
        transfer_values_struct(
          EXPORTING
            json_element = json_element
            options      = options
          CHANGING value = value
        ).
      WHEN cl_abap_typedescr=>kind_table.
        transfer_values_table(
          EXPORTING
            json_element = json_element
            options      = options
            reftype      = reftype
          CHANGING value = value
        ).
      WHEN cl_abap_typedescr=>kind_ref.
        transfer_values_object(
          EXPORTING
            json_element = json_element
            options      = options
            reftype      = reftype
          CHANGING value = value
        ).
    ENDCASE.

  ENDMETHOD.


  METHOD transfer_values_object.

    FIELD-SYMBOLS:
      <children> TYPE t_json_element,
      <child>    TYPE json_element.

    ASSIGN json_element-children->* TO <children>.

    LOOP AT <children> ASSIGNING <child>.

      transfer_value_by_method(
          EXPORTING
            json_element = <child>
            options      = options
            reftype      = reftype
          CHANGING
            value        = value
      ).

      transfer_to_public_attribute(
          EXPORTING
            json_element = <child>
            options      = options
          CHANGING
            value        = value ).

    ENDLOOP.

  ENDMETHOD.


  METHOD transfer_values_struct.

    IF json_element-type NE json_element_type-object. RETURN. ENDIF.

    DATA: lv_attribute_name TYPE string.

    FIELD-SYMBOLS: <children> TYPE t_json_element,
                   <child>    TYPE json_element,
                   <field>    TYPE any.

    ASSIGN json_element-children->* TO <children>.

    LOOP AT <children> ASSIGNING <child>.

      CASE <child>-type.
        WHEN json_element_type-attribute.

          lv_attribute_name = handle_decode_case(
                                value   = <child>-value
                                options = options ).

          ASSIGN COMPONENT lv_attribute_name OF STRUCTURE value TO <field>.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.

          transfer_values(
            EXPORTING
              json_element = <child>
              options      = options
            CHANGING
              value        = <field>
          ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD transfer_values_table.

    DATA: table_type    TYPE REF TO cl_abap_tabledescr,
          struct_type   TYPE REF TO cl_abap_structdescr,
          type_name     TYPE string,
          new_line_data TYPE REF TO data.

    FIELD-SYMBOLS: <children> TYPE t_json_element,
                   <child>    TYPE json_element,
                   <struct>   TYPE any,
                   <itab>     TYPE table.

    ASSIGN value TO <itab>.

    table_type ?= reftype.
    struct_type ?= table_type->get_table_line_type( ).
    type_name = struct_type->get_relative_name( ).

    ASSIGN json_element-children->* TO <children>.

    LOOP AT <children> ASSIGNING <child>.

      CREATE DATA new_line_data TYPE (type_name).
      ASSIGN new_line_data->* TO <struct>.

      transfer_values(
        EXPORTING
          json_element = <child>
          options      = options
        CHANGING
          value        = <struct>
      ).

      APPEND <struct> TO <itab>.

    ENDLOOP.

  ENDMETHOD.


  METHOD transfer_value_by_method.

    IF options-use_objs_methods NE abap_true. RETURN. ENDIF.

    DATA: refdescr    TYPE REF TO cl_abap_refdescr,
          objdescr    TYPE REF TO cl_abap_objectdescr,
          lt_methods  TYPE abap_methdescr_tab,
          method_name TYPE string,
          o_obj       TYPE REF TO object,
          lw_param    TYPE abap_parmbind,
          lt_params   TYPE abap_parmbind_tab.

    FIELD-SYMBOLS: <method>      LIKE LINE OF objdescr->methods,
                   <param_descr> TYPE abap_parmdescr,
                   <value>       TYPE json_element,
                   <children>    TYPE t_json_element.

    refdescr ?= reftype.
    objdescr ?= refdescr->get_referenced_type( ).
    o_obj     = value.
    ASSIGN json_element-children->* TO <children>.

    lt_methods = objdescr->methods.
    SORT lt_methods BY name visibility.

    method_name = handle_decode_case(
                  value   = json_element-value
                  options = options
              ).

    CONCATENATE 'SET_' method_name
           INTO method_name.

    READ TABLE lt_methods ASSIGNING <method>
        WITH KEY name       = method_name
                 visibility = cl_abap_objectdescr=>public BINARY SEARCH.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    IF lines( <method>-parameters ) > 1.
      RETURN.
    ENDIF.

    READ TABLE <method>-parameters
      ASSIGNING <param_descr> INDEX 1.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE <children> ASSIGNING <value> INDEX 1.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    lw_param-name   = <param_descr>-name.
    lw_param-kind   = cl_abap_objectdescr=>exporting.
    GET REFERENCE OF <value>-value INTO lw_param-value.
    INSERT lw_param INTO TABLE lt_params.

    CALL METHOD o_obj->(<method>-name)
      PARAMETER-TABLE lt_params.

  ENDMETHOD.


  METHOD transfer_value_string.

    FIELD-SYMBOLS: <children> TYPE t_json_element,
                   <child>    TYPE json_element.

    ASSIGN json_element-children->* TO <children>.

    READ TABLE <children> ASSIGNING <child> INDEX 1.

    IF reftype->get_relative_name( ) EQ 'TIMESTAMP'.
      value = timestamp_iso_to_sap( <child>-value ).
      RETURN.
    ENDIF.

    CASE reftype->type_kind.
      WHEN cl_abap_typedescr=>typekind_date.
        value = date_iso_to_sap( <child>-value ).
      WHEN cl_abap_typedescr=>typekind_time.
        value = time_iso_to_sap( <child>-value ).
      WHEN OTHERS.
        CASE <child>-type.
          WHEN json_element_type-value_boolean.

            CASE <child>-value.
              WHEN 'false'.
                value = abap_false.
              WHEN 'true'.
                value = abap_true.
            ENDCASE.

          WHEN OTHERS.
            value = <child>-value.
        ENDCASE.
    ENDCASE.

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


  METHOD value_is_valid.

    IF value IS INITIAL AND options-keep_empty_values EQ abap_false.
      RETURN.
    ENDIF.
    result = abap_true.
  ENDMETHOD.

  METHOD timestamp_iso_to_sap.
    DATA: date_iso TYPE string,
          time_iso TYPE string,
          tsc(14)  TYPE c.

    SPLIT timestamp AT 'T' INTO date_iso time_iso.
    CHECK sy-subrc = 0.

    tsc(8) = date_iso_to_sap( date_iso ).
    tsc+8(6) = time_iso_to_sap( time_iso ).
    result = tsc.
  ENDMETHOD.


  METHOD date_iso_to_sap.
    DATA: year(4)  TYPE n,
          month(2) TYPE n,
          day(2)   TYPE n.

    "// ISO-8601 allowed formats:
    "//  YYYY-MM-DD or YYYYMMDD or YYYY-MM
    FIND REGEX '(\d{4})-?(\d{2})-?(\d{2})?' IN date_iso
      SUBMATCHES year month day.
    IF year IS INITIAL AND
       month IS INITIAL AND
       day IS INITIAL.
      RETURN.
    ENDIF.
    IF day IS INITIAL.
      day = 1.
    ENDIF.

    CONCATENATE year month day INTO result.

  ENDMETHOD.


  METHOD time_iso_to_sap.
    DATA: hour(2) TYPE n,
          min(2)  TYPE n,
          sec(2)  TYPE n.

    "// ISO-8601 allowed formats:
    "//  hh:mm:ss or hh:mm or hhmmss or hhmm or hh
    FIND REGEX '(\d{2}):?(\d{2})?:?(\d{2})?' IN time_iso
      SUBMATCHES hour min sec.

    CONCATENATE hour min sec INTO result.

  ENDMETHOD.

ENDCLASS.
