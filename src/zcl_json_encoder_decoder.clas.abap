CLASS zcl_json_encoder_decoder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS: abap.

    METHODS
      constructor.

    METHODS
      encode
        IMPORTING value          TYPE any
                  element_config TYPE REF TO zcl_json_element_config
        RETURNING VALUE(result)  TYPE string.

    METHODS
      decode
        IMPORTING json_string    TYPE clike
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  result         TYPE any.

    METHODS
      normalize_array_attribute
        IMPORTING attribute_name TYPE csequence
                  json_string    TYPE csequence
        RETURNING VALUE(result)  TYPE string.

  PROTECTED SECTION.

    METHODS insert_string_at
      IMPORTING input_string  TYPE csequence
                element       TYPE csequence
                index         TYPE i
      RETURNING VALUE(result) TYPE string.

  PRIVATE SECTION.

    CONSTANTS c_boolean_types TYPE string VALUE '\TYPE-POOL=ABAP\TYPE=ABAP_BOOL#\TYPE=BOOLEAN#\TYPE=BOOLE_D#\TYPE=XFELD'.

    CONSTANTS:
      BEGIN OF json_element_type,
        object        TYPE char1 VALUE 'o',
        array         TYPE char1 VALUE 'r',
        attribute     TYPE char1 VALUE 'a',
        special_value TYPE char1 VALUE 'b',
        name          TYPE char1 VALUE 'n',
      END OF json_element_type.

    TYPES:
      BEGIN OF json_element,
        type     TYPE char1,
        name     TYPE string,
        value    TYPE string,
        children TYPE REF TO data,
      END OF json_element,

      t_json_element TYPE STANDARD TABLE OF json_element WITH DEFAULT KEY.

    DATA: patterns     TYPE STANDARD TABLE OF REF TO cl_abap_regex,
          number_utils TYPE REF TO zcl_number_utils.

    METHODS
      encode_value
        IMPORTING value          TYPE any
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  result         TYPE string.

    METHODS
      encode_numeric
        IMPORTING value          TYPE any
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  result         TYPE string.

    METHODS
      encode_time_to_iso
        IMPORTING value          TYPE any
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  result         TYPE string.

    METHODS
      encode_date_sap_to_iso
        IMPORTING value          TYPE dats
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  result         TYPE string.

    METHODS
      encode_time_sap_to_iso
        IMPORTING value          TYPE tims
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  result         TYPE string.
    METHODS
      type_is_boolean
        IMPORTING type          TYPE REF TO cl_abap_typedescr
        RETURNING VALUE(result) TYPE abap_bool.

    METHODS
      encode_abap_bool
        IMPORTING value          TYPE any
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  result         TYPE string.

    METHODS
      encode_simple_value
        IMPORTING value          TYPE any
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  result         TYPE string.

    METHODS
      run_conversion_exit_output
        IMPORTING value  TYPE any
                  type   TYPE REF TO cl_abap_typedescr
        CHANGING  result TYPE string.

    METHODS
      encode_struct
        IMPORTING value          TYPE any
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  result         TYPE string.

    METHODS
      encode_table
        IMPORTING value          TYPE any
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  result         TYPE string.

    METHODS
      value_is_valid
        IMPORTING value          TYPE any
                  element_config TYPE REF TO zcl_json_element_config
        RETURNING VALUE(result)  TYPE abap_bool.

    METHODS
      transfer_values
        IMPORTING json_element   TYPE zcl_json_encoder_decoder=>json_element
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  value          TYPE any.

    METHODS
      transfer_values_struct
        IMPORTING json_element   TYPE zcl_json_encoder_decoder=>json_element
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  value          TYPE any.

    METHODS
      transfer_value_string
        IMPORTING json_element   TYPE zcl_json_encoder_decoder=>json_element
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  value          TYPE any.
    METHODS
      transfer_values_table
        IMPORTING json_element   TYPE zcl_json_encoder_decoder=>json_element
                  element_config TYPE REF TO zcl_json_element_config
        CHANGING  value          TYPE any.

    METHODS
      timestamp_iso_to_sap
        IMPORTING timestamp     TYPE string
        RETURNING VALUE(result) TYPE timestamp.

    METHODS
      date_iso_to_sap
        IMPORTING date_iso      TYPE string
        RETURNING VALUE(result) TYPE string.

    METHODS
      time_iso_to_sap
        IMPORTING time_iso      TYPE string
        RETURNING VALUE(result) TYPE string.

    METHODS
      remove_special_characters
        IMPORTING input         TYPE clike
        RETURNING VALUE(result) TYPE string.

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
    CREATE OBJECT me->number_utils.

  ENDMETHOD.


  METHOD date_iso_to_sap.
    DATA: year(4)  TYPE n,
          month(2) TYPE n,
          day(2)   TYPE n.

    "// ISO-8601 allowed formats:
    "//  YYYY-MM-DD or YYYYMMDD or YYYY-MM
    FIND REGEX '(\d{4})-?(\d{2})-?(\d{2})?' IN date_iso
      SUBMATCHES year month day.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    IF day IS INITIAL.
      day = 1.
    ENDIF.

    CONCATENATE year month day INTO result.

  ENDMETHOD.


  METHOD decode.

    DATA: condensed_string TYPE string,
          lw_decoded       TYPE json_element.

    condensed_string = json_string.
    REPLACE ALL OCCURRENCES OF:
        cl_abap_char_utilities=>newline IN condensed_string WITH '',
        cl_abap_char_utilities=>cr_lf IN condensed_string WITH '',
        cl_abap_char_utilities=>horizontal_tab IN condensed_string WITH '',
        cl_abap_char_utilities=>form_feed IN condensed_string WITH '',
        cl_abap_char_utilities=>vertical_tab IN condensed_string WITH '',
        REGEX '((\s)("|\{|\[|:|\}|]|,))' IN condensed_string WITH '$3'.

    CONDENSE condensed_string.

    DATA: scanner TYPE REF TO scanner.
    CREATE OBJECT scanner.

    IF scanner->valid( condensed_string ) EQ abap_false.
      RETURN.
    ENDIF.
    lw_decoded = scanner->get_json_element_tree( ).

    transfer_values(
       EXPORTING json_element   = lw_decoded
                 element_config = element_config
       CHANGING value = result
    ).

  ENDMETHOD.


  METHOD encode.

    me->encode_value(
        EXPORTING value          = value
                  element_config = element_config
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
    CONDENSE boolean_value.
    CONCATENATE result boolean_value INTO result.

  ENDMETHOD.


  METHOD encode_date_sap_to_iso.

    IF value_is_valid(
       value   = value
       element_config = element_config ) EQ abap_false.
      RETURN.
    ENDIF.

    IF value IS NOT INITIAL OR element_config->required EQ abap_true.
      CONCATENATE '"' result value(4) '-' value+4(2) '-' value+6(2) '"' INTO result.
    ENDIF.

  ENDMETHOD.


  METHOD encode_numeric.

    DATA: formatted_value TYPE string.

    IF value_is_valid(
       value   = value
       element_config = element_config ) EQ abap_false.
      RETURN.
    ENDIF.

    formatted_value = value.
    TRANSLATE formatted_value USING '- + '.
    CONDENSE formatted_value NO-GAPS.
    IF value < 0.
      CONCATENATE '-' formatted_value INTO formatted_value.
    ENDIF.

    IF formatted_value IS NOT INITIAL OR element_config->required EQ abap_true.
      CONCATENATE result
                  formatted_value
             INTO result.
    ENDIF.

  ENDMETHOD.


  METHOD encode_simple_value.

    DATA: lv_value TYPE string.

    IF value_is_valid(
       value   = value
       element_config = element_config ) EQ abap_false.
      RETURN.
    ENDIF.

    lv_value = value.

    IF element_config->use_conversion_exit EQ abap_true.
      run_conversion_exit_output(
        EXPORTING
            value = value
            type  = element_config->type
        CHANGING result = lv_value ).
    ENDIF.

    IF lv_value IS NOT INITIAL OR element_config->required EQ abap_true.
      lv_value = cl_http_utility=>escape_javascript(
                   unescaped   = lv_value
*                   inside_html = abap_false
                 ).
      REPLACE ALL OCCURRENCES OF REGEX '(\\'')' IN lv_value WITH ''''.
      CONCATENATE result '"' lv_value '"' INTO result RESPECTING BLANKS.
    ENDIF.

  ENDMETHOD.


  METHOD encode_struct.

    DATA: struct       TYPE REF TO cl_abap_structdescr,
          json_value   TYPE string,
          next         TYPE string,
          field_config TYPE REF TO zcl_json_element_config.

    FIELD-SYMBOLS: <component> LIKE LINE OF struct->components,
                   <field>     TYPE any.

    struct ?= element_config->type.

    next = '{'.
    LOOP AT struct->components ASSIGNING <component>.
      ASSIGN COMPONENT <component>-name OF STRUCTURE value TO <field>.

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      field_config = element_config->get_child_by_abap_name( <component>-name ).

      encode_value(
        EXPORTING
            value = <field>
            element_config = field_config
        CHANGING
            result = json_value ).

      IF json_value IS NOT INITIAL.
        CONCATENATE result next '"' field_config->ext_name '":' json_value INTO result RESPECTING BLANKS.
        next = ','.
        FREE: json_value.
      ENDIF.

    ENDLOOP.

    IF next EQ '{'.
      IF element_config->required EQ abap_true.
        CONCATENATE result '{}' INTO result RESPECTING BLANKS.
      ENDIF.
    ELSE.
      CONCATENATE result '}' INTO result RESPECTING BLANKS.
    ENDIF.

  ENDMETHOD.


  METHOD encode_table.

    DATA: table         TYPE REF TO cl_abap_tabledescr,
          lines_json    TYPE TABLE OF string,
          line          TYPE string,
          struct_config TYPE REF TO zcl_json_element_config.

    FIELD-SYMBOLS: <table> TYPE ANY TABLE,
                   <line>  TYPE any.

    "// Encode all table lines
    table ?= element_config->type.

    ASSIGN value TO <table>.

    struct_config = element_config->get_child_by_abap_name( abap_name = 'TLINE' ).

    LOOP AT <table> ASSIGNING <line>.
      encode_value(
        EXPORTING
            value = <line>
            element_config = struct_config
        CHANGING
            result = line ).

      IF line IS NOT INITIAL.
        APPEND line TO lines_json.
      ENDIF.

      FREE: line.

    ENDLOOP.

    IF lines( lines_json ) > 0 OR element_config->required EQ abap_true.
      CONCATENATE: result '[' INTO result,
                   LINES OF lines_json INTO line SEPARATED BY ',',
                   result line ']' INTO result RESPECTING BLANKS.
    ENDIF.

  ENDMETHOD.


  METHOD encode_time_sap_to_iso.

    IF value_is_valid(
       value   = value
       element_config = element_config ) EQ abap_false.
      RETURN.
    ENDIF.

    CONCATENATE result '"' value(2) ':' value+2(2) ':' value+4(2) '"' INTO result.
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
       element_config = element_config ) EQ abap_false.
      RETURN.
    ENDIF.

    ts_split = tsc = value.
    encode_date_sap_to_iso(
      EXPORTING
        value  = ts_split-date
        element_config = element_config
      CHANGING
        result = date_iso
    ).
    encode_time_sap_to_iso(
      EXPORTING
        value  = ts_split-time
        element_config = element_config
      CHANGING
        result = time_iso
    ).
    REPLACE ALL OCCURRENCES OF '"' IN: date_iso WITH '',
                                       time_iso WITH ''.

    CONCATENATE '"' result date_iso 'T' time_iso '"' INTO result.
  ENDMETHOD.


  METHOD encode_value.

    CASE element_config->type->type_kind.
      WHEN  cl_abap_typedescr=>typekind_struct1 OR
            cl_abap_typedescr=>typekind_struct2.
        encode_struct(
          EXPORTING
            value = value
            element_config = element_config
          CHANGING
            result = result
        ).
      WHEN  cl_abap_typedescr=>typekind_table.
        encode_table(
          EXPORTING
            value = value
            element_config = element_config
          CHANGING
            result = result
        ).

      WHEN  cl_abap_typedescr=>typekind_date.
        encode_date_sap_to_iso(
          EXPORTING
            value = value
            element_config = element_config
          CHANGING
            result = result ).

      WHEN  cl_abap_typedescr=>typekind_time.
        encode_time_sap_to_iso(
          EXPORTING
            value = value
            element_config = element_config
          CHANGING
            result = result ).
      WHEN OTHERS.
        IF element_config->type->get_relative_name( ) EQ 'TIMESTAMP'.
          encode_time_to_iso(
            EXPORTING
              value = value
              element_config = element_config
            CHANGING
              result = result ).
        ELSEIF type_is_boolean( element_config->type ) EQ abap_true.
          encode_abap_bool(
            EXPORTING
                value = value
                element_config = element_config
            CHANGING
                result = result ).
        ELSEIF me->number_utils->is_numeric( value ) EQ abap_true.
          encode_numeric(
              EXPORTING
                  value = value
                  element_config = element_config
              CHANGING
                  result = result ).
        ELSE.

          encode_simple_value(
            EXPORTING
                value = value
                element_config = element_config
            CHANGING
                result = result ).
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD insert_string_at.

    DATA: prefix TYPE string,
          suffix TYPE string.

    result = input_string.
    prefix = result+0(index).
    suffix = result+index.
    CONCATENATE prefix element suffix INTO result.

  ENDMETHOD.


  METHOD normalize_array_attribute.

    DATA: open_objects        TYPE match_result_tab,
          close_objects       TYPE match_result_tab,
          search_result       TYPE match_result_tab,
          search_clause       TYPE string,
          bracket_position    TYPE i,
          nested_opens        TYPE i,
          close_bracket_index TYPE i.

    FIELD-SYMBOLS: <search_info> TYPE match_result.

    result = json_string.

    FIND ALL OCCURRENCES OF REGEX:
        '\{' IN json_string RESULTS open_objects,
        '\}' IN json_string RESULTS close_objects.

    IF lines( open_objects ) NE lines( close_objects ).
      RETURN.
    ENDIF.

    CONCATENATE '("' attribute_name '":\{)' INTO search_clause.

    FIND ALL OCCURRENCES OF REGEX search_clause IN result RESULTS search_result.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    "Posição na string onde começa o termo de busca + tamanho da busca
    "Take the first result of regex result
    READ TABLE search_result ASSIGNING <search_info> INDEX 1.

    "Minus 1 of escape bracket character.
    bracket_position = <search_info>-offset + <search_info>-length - 1.

    "checar qual é a quantidade de chaves abertas até a posição do termo de busca
    READ TABLE open_objects TRANSPORTING NO FIELDS WITH KEY offset = bracket_position.

    nested_opens = lines( open_objects ) - sy-tabix.

    "incluir o fechamento do array da mesma quantidade de chaves de fechamento do objeto
    DELETE close_objects WHERE offset < bracket_position.

    close_bracket_index = 1 + nested_opens.

    READ TABLE close_objects ASSIGNING <search_info> INDEX close_bracket_index.

    close_bracket_index = <search_info>-offset + 1.

    result = insert_string_at( input_string = result
                               element      = ']'
                               index        = close_bracket_index ).

    result = insert_string_at( input_string = result
                               element      = '['
                               index        = bracket_position ).
  ENDMETHOD.


  METHOD remove_special_characters.

    DATA: input_string    TYPE fist-searchw,
          output_string   TYPE fist-searchw,
          specharsear(50) VALUE ''' < > ! " & / = ? : ; , . - ( ) # # % ^ $ | ~ @ '.

    input_string = input.

    TRANSLATE input_string USING specharsear.
    CONDENSE  input_string NO-GAPS.
    output_string = input_string.
    "WD = WORD.

    SET EXTENDED CHECK OFF.

    WHILE output_string CA 'ÄÖÜß'.
      REPLACE 'Ä' WITH 'AE' INTO output_string.
      REPLACE 'Ö' WITH 'OE' INTO output_string.
      REPLACE 'Ü' WITH 'UE' INTO output_string.
      REPLACE 'ß' WITH 'SS' INTO output_string.
    ENDWHILE.

    SET EXTENDED CHECK ON.

    result = output_string.

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


  METHOD timestamp_iso_to_sap.
    DATA: date_iso TYPE string,
          time_iso TYPE string,
          tsc(14)  TYPE c.

    SPLIT timestamp AT 'T' INTO date_iso time_iso.
    CHECK sy-subrc = 0.

    tsc(8) = date_iso_to_sap( date_iso  ).
    tsc+8(6) = time_iso_to_sap( time_iso ).
    result = tsc.
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


  METHOD transfer_values.

    CASE element_config->type->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        transfer_value_string(
          EXPORTING
            json_element   = json_element
            element_config = element_config
          CHANGING
            value        = value
        ).
      WHEN cl_abap_typedescr=>kind_struct.
        transfer_values_struct(
          EXPORTING
            json_element   = json_element
            element_config = element_config
          CHANGING value = value
        ).
      WHEN cl_abap_typedescr=>kind_table.
        transfer_values_table(
          EXPORTING
            json_element = json_element
            element_config = element_config
          CHANGING value = value
        ).
    ENDCASE.

  ENDMETHOD.


  METHOD transfer_values_struct.

    DATA: lv_json_name TYPE string,
          item_config  TYPE REF TO zcl_json_element_config.

    FIELD-SYMBOLS: <children> TYPE t_json_element,
                   <child>    TYPE json_element,
                   <field>    TYPE any.

    ASSIGN json_element-children->* TO <children>.

    READ TABLE <children> ASSIGNING <child> INDEX 1.
    IF sy-subrc EQ 0 AND <child>-type = json_element_type-object.
      ASSIGN <child>-children->* TO <children>.
    ENDIF.

    LOOP AT <children> ASSIGNING <child>.
      lv_json_name = remove_special_characters( <child>-name ).
      item_config = element_config->get_child_by_external_name( lv_json_name ).

      IF item_config IS NOT BOUND.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT item_config->abap_name OF STRUCTURE value TO <field>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      transfer_values(
        EXPORTING
          json_element = <child>
          element_config = item_config
        CHANGING
          value        = <field>
      ).

    ENDLOOP.

  ENDMETHOD.


  METHOD transfer_values_table.

    DATA: table_type    TYPE REF TO cl_abap_tabledescr,
          struct_type   TYPE REF TO cl_abap_typedescr,
          type_name     TYPE string,
          struct_config TYPE REF TO zcl_json_element_config,
          new_line_data TYPE REF TO data,
          is_ddic_type  TYPE abap_bool.

    FIELD-SYMBOLS: <children> TYPE t_json_element,
                   <child>    TYPE json_element,
                   <struct>   TYPE any,
                   <itab>     TYPE table.

    ASSIGN value TO <itab>.

    table_type ?= element_config->type.
    struct_type ?= table_type->get_table_line_type( ).
    ASSIGN json_element-children->* TO <children>.
    READ TABLE <children> ASSIGNING <child> INDEX 1.
    IF sy-subrc EQ 0 AND <child>-type = json_element_type-array.
      ASSIGN <child>-children->* TO <children>.
    ENDIF.

    struct_config = element_config->get_child_by_abap_name( 'TLINE' ).

    LOOP AT <children> ASSIGNING <child>.

      TRY.

          CASE struct_type->kind.
            WHEN struct_type->kind_struct OR struct_type->kind_elem.
              is_ddic_type = struct_type->is_ddic_type( ).
              IF is_ddic_type = abap_true.
                type_name = struct_type->get_relative_name( ).
              ELSE.
                type_name = struct_type->absolute_name.
              ENDIF.
              ASSIGN new_line_data->* TO <struct>.
              CREATE DATA new_line_data TYPE (type_name).
              ASSIGN new_line_data->* TO <struct>.
          ENDCASE.

        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      transfer_values(
        EXPORTING
          json_element   = <child>
          element_config = struct_config
        CHANGING
          value = <struct>
      ).

      APPEND <struct> TO <itab>.

      CLEAR: new_line_data.

    ENDLOOP.

  ENDMETHOD.


  METHOD transfer_value_string.

    IF element_config->type->get_relative_name( ) EQ 'TIMESTAMP'.
      value = timestamp_iso_to_sap( json_element-value ).
      RETURN.
    ENDIF.

    CASE element_config->type->type_kind.
      WHEN cl_abap_typedescr=>typekind_date.
        value = date_iso_to_sap( json_element-value ).
      WHEN cl_abap_typedescr=>typekind_time.
        value = time_iso_to_sap( json_element-value ).
      WHEN OTHERS.
        CASE json_element-type.
          WHEN json_element_type-special_value.
            CASE json_element-value.
              WHEN 'false'.
                value = abap_false.
              WHEN 'true'.
                value = abap_true.
              WHEN 'null'.
                CLEAR: value.
            ENDCASE.
          WHEN OTHERS.
            value = json_element-value.
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
    IF value IS INITIAL AND element_config->required EQ abap_false.
      RETURN.
    ENDIF.
    result = abap_true.
  ENDMETHOD.
ENDCLASS.
