*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS scanner DEFINITION.

  PUBLIC SECTION.

    TYPE-POOLS: abap.

    CONSTANTS:
      BEGIN OF json_element_type,
        object        TYPE char1 VALUE 'o',
        array         TYPE char1 VALUE 'r',
        attribute     TYPE char1 VALUE 'a',
        value_string  TYPE char1 VALUE 's',
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

    CONSTANTS:
      BEGIN OF steps,
        begin_string          TYPE string VALUE 'BEGIN_STRING',
        begin_value           TYPE string VALUE 'BEGIN_VALUE',
        begin_string_or_empty TYPE string VALUE 'BEGIN_STRING_OR_EMPTY',
        begin_value_or_empty  TYPE string VALUE 'BEGIN_VALUE_OR_EMPTY',
        end_top               TYPE string VALUE 'END_TOP',
        end_value             TYPE string VALUE 'END_VALUE',
        in_string             TYPE string VALUE 'IN_STRING',
        in_string_esc         TYPE string VALUE 'IN_STRING_ESC',
        neg                   TYPE string VALUE 'NEG',
        zero                  TYPE string VALUE 'ZERO',
        numeric               TYPE string VALUE 'NUMERIC',
        dot                   TYPE string VALUE 'DOT',
        dot_zero              TYPE string VALUE 'DOT_ZERO',
        exp                   TYPE string VALUE 'EXP',
        esign                 TYPE string VALUE 'ESIGN',
        exp_zero              TYPE string VALUE 'EXP_ZERO',
        t                     TYPE string VALUE 'T',
        tr                    TYPE string VALUE 'TR',
        tru                   TYPE string VALUE 'TRU',
        f                     TYPE string VALUE 'F',
        fa                    TYPE string VALUE 'FA',
        fal                   TYPE string VALUE 'FAL',
        fals                  TYPE string VALUE 'FALS',
        n                     TYPE string VALUE 'N',
        nu                    TYPE string VALUE 'NU',
        nul                   TYPE string VALUE 'NUL',
      END OF steps,

      BEGIN OF parse_states,
        object_key   TYPE i VALUE 1,
        object_value TYPE i VALUE 2,
        array_value  TYPE i VALUE 3,
      END OF parse_states,

      BEGIN OF scan_result,
        continue      TYPE i VALUE 1,
        begin_literal TYPE i VALUE 2,
        begin_object  TYPE i VALUE 3,
        object_key    TYPE i VALUE 4,
        object_value  TYPE i VALUE 5,
        end_object    TYPE i VALUE 6,
        begin_array   TYPE i VALUE 7,
        array_value   TYPE i VALUE 8,
        end_array     TYPE i VALUE 9,
        skip_space    TYPE i VALUE 10,
        end           TYPE i VALUE 11,
        error         TYPE i VALUE 12,
      END OF scan_result.

    METHODS:

      valid
        IMPORTING json            TYPE string
        RETURNING VALUE(is_valid) TYPE abap_bool,

      get_json_element_tree
        RETURNING VALUE(result) TYPE scanner=>json_element.

  PROTECTED SECTION.


  PRIVATE SECTION.

    DATA: is_end_top              TYPE abap_bool,
          no_characters_processed TYPE i,
          error                   TYPE string,
          step_name               TYPE string,
          parse_state             TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
          actual_element          TYPE REF TO data,
          elements                TYPE STANDARD TABLE OF REF TO data WITH DEFAULT KEY.

    METHODS:
      check_is_valid
        IMPORTING json          TYPE string
        RETURNING VALUE(result) TYPE string,

      reset,

      eof
        RETURNING VALUE(result) TYPE i,

      begin_value
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      begin_string_or_empty
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      begin_value_or_empty
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      is_space
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE abap_bool,

      begin_string
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      end_value
        IMPORTING
          character     TYPE c
        RETURNING
          VALUE(result) TYPE i,

      end_top
        IMPORTING
          character     TYPE c
        RETURNING
          VALUE(result) TYPE i,

      gen_error
        IMPORTING
          character     TYPE c
          context       TYPE string
        RETURNING
          VALUE(result) TYPE i,

      change_last_parse_state
        IMPORTING
          parse_state TYPE i,

      pop_parse_state,

      perform_step
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      in_string
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      in_string_esc
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      neg
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      zero
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      numeric
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      dot
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      dot_zero
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      exp
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      esign
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      exp_zero
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      t
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      tr
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      tru
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      f
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      fa
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      fal
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      fals
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      n
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      nu
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      nul
        IMPORTING character     TYPE c
        RETURNING VALUE(result) TYPE i,

      append_string
        IMPORTING
          value_to_append TYPE clike,

      create_json_element
        IMPORTING type          TYPE char1
        RETURNING VALUE(result) TYPE REF TO data,

      clear_json_tree,

      has_actual_element
        IMPORTING type          TYPE char1
        RETURNING VALUE(result) TYPE abap_bool,

      initialize_json_elements
        CHANGING
          json_element TYPE scanner=>json_element,

      append_json_element
        IMPORTING
          type TYPE char1,

      change_json_element_type
        IMPORTING
          type TYPE char1,

      pop_json_element,

      get_actual_json_element
        RETURNING VALUE(result) TYPE scanner=>json_element,

      get_actual_json_element_type
        RETURNING VALUE(result) TYPE char1,

      process_string_value
        IMPORTING
            json type string.

ENDCLASS.

CLASS scanner IMPLEMENTATION.

  METHOD valid.
    IF me->check_is_valid( json ) IS INITIAL.
      is_valid = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD check_is_valid.

    DATA: json_length        TYPE i,
          result_step        TYPE i,
          character          TYPE c,
          character_position TYPE i.

    json_length = strlen( json ).
    me->reset( ).
    WHILE me->no_characters_processed < json_length.

      ADD 1 TO me->no_characters_processed.
      character_position = me->no_characters_processed - 1.
      character = json+character_position(1).
      me->perform_step( character = character ).

      IF result_step EQ scan_result-error.
        me->clear_json_tree( ).
        result = me->error.
        RETURN.
      ENDIF.

      IF me->step_name EQ steps-in_string.
        me->process_string_value( json ).
      ENDIF.

    ENDWHILE.

    IF me->eof( ) EQ scan_result-error.
      result  = me->error.
      RETURN.
    ENDIF.

    RETURN. "Similar return initial string.

  ENDMETHOD.


  METHOD reset.
    me->step_name = steps-begin_value.
    FREE: me->error,
          me->parse_state,
          me->no_characters_processed.
    me->clear_json_tree( ).
    me->is_end_top = abap_false.
  ENDMETHOD.


  METHOD eof.

    IF me->error IS NOT INITIAL.
      result = scan_result-error.
      RETURN.
    ENDIF.

    IF me->is_end_top EQ abap_true.
      result = scan_result-end.
      RETURN.
    ENDIF.

    me->perform_step( character = ' ' ).

    IF me->is_end_top EQ abap_true.
      result = scan_result-end.
      RETURN.
    ENDIF.

    IF me->error IS INITIAL.
      me->error = 'unexpected end of JSON input'.
    ENDIF.

    result = scan_result-error.

  ENDMETHOD.

  METHOD begin_value.

    IF me->is_space( character = character ) EQ abap_true.
      result = scan_result-skip_space.
      RETURN.
    ENDIF.

    CASE character.
      WHEN '{'.
        me->step_name = steps-begin_string_or_empty.
        APPEND parse_states-object_key TO me->parse_state.
        result = scan_result-begin_object.
        me->append_json_element( type = json_element_type-object ).
        RETURN.
      WHEN '['.
        me->step_name = steps-begin_value_or_empty.
        APPEND parse_states-array_value TO me->parse_state.
        result = scan_result-begin_array.
        me->append_json_element( type = json_element_type-array ).
        RETURN.
      WHEN '"'.
        me->step_name = steps-in_string.
        result = scan_result-begin_literal.
        IF me->get_actual_json_element_type( ) EQ json_element_type-array.
          me->append_json_element( type = json_element_type-attribute ).
        ELSE.
          me->change_json_element_type( type = json_element_type-attribute ).
        ENDIF.
        RETURN.
      WHEN '-'.
        me->step_name = steps-neg.
        result = scan_result-begin_literal.
        me->change_json_element_type( type = json_element_type-attribute ).
        me->append_string( character ).
        RETURN.
      WHEN '0'.
        me->step_name = steps-zero.
        result = scan_result-begin_literal.
        me->change_json_element_type( type = json_element_type-attribute ).
        me->append_string( character ).
        RETURN.
      WHEN 't'.
        me->step_name = steps-t.
        result = scan_result-begin_literal.
        me->change_json_element_type( type = json_element_type-attribute ).
        me->append_string( character ).
        RETURN.
      WHEN 'f'.
        me->step_name = steps-f.
        result = scan_result-begin_literal.
        me->change_json_element_type( type = json_element_type-attribute ).
        me->append_string( character ).
        RETURN.
      WHEN 'n'.
        me->step_name = steps-n.
        result = scan_result-begin_literal.
        me->change_json_element_type( type = json_element_type-attribute ).
        me->append_string( character ).
        RETURN.
    ENDCASE.

    IF character CO '123456789'.
      me->step_name = steps-numeric.
      result = scan_result-begin_literal.
      me->change_json_element_type( type = json_element_type-attribute ).
      me->append_string( character ).
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'looking for beginning of value' ).

  ENDMETHOD.


  METHOD is_space.
    IF character EQ ' '.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD begin_string_or_empty.

    IF me->is_space( character = character ) EQ abap_true.
      result = scan_result-skip_space.
      RETURN.
    ENDIF.

    IF character EQ '}'.
      me->change_last_parse_state( parse_states-object_value ).
      result = me->end_value( character = character ).
      RETURN.
    ENDIF.

    result = me->begin_string( character = character ).

  ENDMETHOD.


  METHOD begin_string.

    IF me->is_space( character = character ) EQ abap_true.
      result = scan_result-skip_space.
      RETURN.
    ENDIF.

    IF character EQ '"'.
      me->step_name = steps-in_string.
      result = scan_result-begin_literal.
      me->append_json_element( type = json_element_type-name ).
      RETURN.
    ENDIF.

    result = me->gen_error( character = character
                            context   = 'looking for beginning of object key string' ).

  ENDMETHOD.


  METHOD end_value.

    IF lines( me->parse_state ) EQ 0.
      me->step_name = steps-end_top.
      me->is_end_top = abap_true.
      result = me->end_top( character = character ).
      RETURN.
    ENDIF.

    IF me->is_space( character = character ) EQ abap_true.
      me->step_name = steps-end_value.
      result = scan_result-skip_space.
      RETURN.
    ENDIF.

    DATA: lv_parse_state         TYPE i,
          json_element           TYPE json_element,
          last_parse_state_index TYPE i.

    last_parse_state_index = lines( me->parse_state ).

    READ TABLE me->parse_state INTO lv_parse_state INDEX last_parse_state_index.

    CASE lv_parse_state.
      WHEN parse_states-object_key.
        IF character EQ ':'.
          me->change_last_parse_state( parse_states-object_value ).
*          me->change_json_element_type( type = scanner=>json_element_type-object ).
          me->step_name = steps-begin_value.
          result = scan_result-object_key.
          RETURN.
        ENDIF.

        result = me->gen_error(
             character = character
             context   = 'after object key'
         ).
        RETURN.
      WHEN parse_states-object_value.
        IF character EQ ','.
          me->change_last_parse_state( parse_states-object_key ).
          me->pop_json_element( ).
          IF me->get_actual_json_element_type( ) EQ json_element_type-name.
            me->pop_json_element( ).
          ENDIF.
          me->step_name = steps-begin_string.
          result = scan_result-object_value.
          RETURN.
        ENDIF.

        IF character EQ '}'.
          me->pop_parse_state( ).
          me->pop_json_element( ). "Up to object
          json_element = get_actual_json_element( ).
          IF json_element-type EQ json_element_type-attribute OR
             json_element-type EQ json_element_type-name.
            me->pop_json_element( ).
          ENDIF.
          result = scan_result-object_value.
          RETURN.
        ENDIF.

        result = me->gen_error(
             character = character
             context   = 'after object key:value pair'
         ).
        RETURN.
      WHEN parse_states-array_value.

        IF me->get_actual_json_element_type( ) EQ json_element_type-attribute.
          me->change_json_element_type( json_element_type-value_string ).
          json_element = me->get_actual_json_element( ).
          json_element-value = json_element-name.
          FREE: json_element-name.
        ENDIF.

        IF character EQ ','.
          me->step_name = steps-begin_value.
          result = scan_result-array_value.
          me->pop_json_element( ).
          RETURN.
        ENDIF.

        IF character EQ ']'.
          me->pop_parse_state( ).
          me->pop_json_element( ).
          result = scan_result-end_array.
          RETURN.
        ENDIF.

        result = me->gen_error(
             character = character
             context   = 'after array element'
         ).
        RETURN.
    ENDCASE.

    result = me->gen_error( character = character
                            context   = '' ).

  ENDMETHOD.


  METHOD end_top.

    IF me->is_space( character = character ) NE abap_true.
      result = me->gen_error( character = character
                              context   = 'after top-level value' ).
    ENDIF.

    result = scan_result-end.

  ENDMETHOD.


  METHOD gen_error.
    CONCATENATE 'invalid character'
                character
                context
           INTO me->error SEPARATED BY space.

    result = scan_result-error.
  ENDMETHOD.


  METHOD change_last_parse_state.
    DATA: last_parse_state_index TYPE i.
    last_parse_state_index = lines( me->parse_state ).
    MODIFY me->parse_state FROM parse_state INDEX last_parse_state_index.
  ENDMETHOD.


  METHOD pop_parse_state.
    DATA: last_parse_state_index TYPE i.
    last_parse_state_index = lines( me->parse_state ).
    DELETE me->parse_state INDEX last_parse_state_index.

    IF lines( me->parse_state ) EQ 0.
      me->step_name = steps-end_top.
      me->is_end_top = abap_true.
    ELSE.
      me->step_name = steps-end_value.
    ENDIF.
  ENDMETHOD.


  METHOD perform_step.

    CALL METHOD me->(me->step_name)
      EXPORTING
        character = character
      RECEIVING
        result    = result.

  ENDMETHOD.

  METHOD in_string.

    IF character EQ '"'.
      me->step_name = steps-end_value.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    IF character EQ '\'.
      me->step_name = steps-in_string_esc.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    me->append_string( value_to_append = character ).

    result = scan_result-continue.

  ENDMETHOD.

  METHOD begin_value_or_empty.

    IF me->is_space( character = character ) EQ abap_true.
      result = scan_result-skip_space.
      RETURN.
    ENDIF.

    IF character EQ ']'.
      result = me->end_value( character ).
      RETURN.
    ENDIF.

    result = me->begin_value( character ).

  ENDMETHOD.

  METHOD neg.

    IF character EQ '0'.
      me->step_name = steps-zero.
      result = scan_result-continue.
      me->append_string( value_to_append = character ).
      RETURN.
    ENDIF.

    IF character CO '123456789'.
      me->step_name = steps-numeric.
      result = scan_result-continue.
      me->append_string( value_to_append = character ).
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in numeric literal' ).

  ENDMETHOD.

  METHOD zero.

    IF character EQ '.'.
      me->step_name = steps-dot.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    IF character EQ 'e' OR character EQ 'E'.
      me->step_name = steps-exp.
      result = me->scan_result-continue.
      RETURN.
    ENDIF.

    result = me->end_value( character = character ).

  ENDMETHOD.

  METHOD numeric.

    IF character CO '0123456789'.
      me->step_name = steps-numeric.
      result = scan_result-continue.
      me->append_string( character ).
      RETURN.
    ENDIF.

    result = me->zero( character ).

  ENDMETHOD.

  METHOD dot.

    IF character CO '0123456789'.
      me->step_name = steps-dot_zero.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'after decimal point in numeric literal' ).

  ENDMETHOD.

  METHOD dot_zero.

    IF character CO '0123456789'.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    IF character EQ 'e' OR character EQ 'E'.
      me->step_name = steps-exp. "voltar daqui
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->end_value( character = character ).

  ENDMETHOD.

  METHOD exp.

    IF character EQ '+' OR character EQ '-'.
      me->step_name = steps-esign.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->esign( character = character ).

  ENDMETHOD.

  METHOD esign.

    IF character CO '0123456789'.
      me->step_name = steps-exp_zero.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in exponent of numeric literal' ).

  ENDMETHOD.

  METHOD exp_zero.

    IF character CO '0123456789'.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->end_value( character = character ).

  ENDMETHOD.

  METHOD t.

    IF character = 'r'.
      me->append_string( character ).
      me->step_name = steps-tr.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in literal true (expecting ''r'')' ).

  ENDMETHOD.

  METHOD tr.

    IF character = 'u'.
      me->append_string( character ).
      me->step_name = steps-tru.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in literal true (expecting ''u'')' ).

  ENDMETHOD.

  METHOD tru.

    IF character = 'e'.
      me->append_string( character ).
      me->change_json_element_type( type = scanner=>json_element_type-special_value ).
      me->step_name = steps-end_value.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in literal true (expecting ''e'')' ).

  ENDMETHOD.

  METHOD f.

    IF character = 'a'.
      me->append_string( character ).
      me->step_name = steps-fa.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in literal false (expecting ''a'')' ).

  ENDMETHOD.

  METHOD fa.

    IF character = 'l'.
      me->append_string( character ).
      me->step_name = steps-fal.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in literal false (expecting ''l'')' ).

  ENDMETHOD.

  METHOD fal.

    IF character = 's'.
      me->append_string( character ).
      me->step_name = steps-fals.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in literal false (expecting ''s'')' ).

  ENDMETHOD.

  METHOD fals.

    IF character = 'e'.
      me->append_string( character ).
      me->change_json_element_type( type = scanner=>json_element_type-special_value ).
      me->step_name = steps-end_value.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in literal false (expecting ''e'')' ).

  ENDMETHOD.

  METHOD n.

    IF character = 'u'.
      me->append_string( character ).
      me->step_name = steps-nu.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in literal null (expecting ''u'')' ).

  ENDMETHOD.

  METHOD nu.

    IF character = 'l'.
      me->append_string( character ).
      me->step_name = steps-nul.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in literal null (expecting ''l'')' ).

  ENDMETHOD.

  METHOD nul.

    IF character = 'l'.
      me->append_string( character ).
      me->change_json_element_type( type = scanner=>json_element_type-special_value ).
      me->step_name = steps-end_value.
      result = scan_result-continue.
      RETURN.
    ENDIF.

    result = me->gen_error(
             character = character
             context   = 'in literal null (expecting ''l'')' ).

  ENDMETHOD.

  METHOD append_string.

    IF me->actual_element IS NOT BOUND.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS:
      <json_element> TYPE json_element,
      <field>        TYPE any.

    ASSIGN me->actual_element->* TO <json_element>.

    CASE <json_element>-type.
      WHEN scanner=>json_element_type-name.
        ASSIGN <json_element>-name TO <field>.
      WHEN scanner=>json_element_type-attribute.
        ASSIGN <json_element>-value TO <field>.
    ENDCASE.

    CONCATENATE <field> value_to_append INTO <field> RESPECTING BLANKS.

  ENDMETHOD.

  METHOD create_json_element.
    FIELD-SYMBOLS: <json_element> TYPE json_element.
    CREATE DATA: result TYPE json_element.
    ASSIGN result->* TO <json_element>.
    me->initialize_json_elements( CHANGING json_element = <json_element> ).
    <json_element>-type = type.
  ENDMETHOD.


  METHOD get_json_element_tree.

    DATA: ref_first TYPE REF TO data.
    FIELD-SYMBOLS: <result> LIKE result.

    READ TABLE me->elements INTO ref_first INDEX 1.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    ASSIGN ref_first->* TO <result>.
    result = <result>.

  ENDMETHOD.


  METHOD clear_json_tree.
    FREE: me->elements,
          me->actual_element.
  ENDMETHOD.


  METHOD has_actual_element.

    IF me->actual_element IS BOUND.
      result = abap_true.
      RETURN.
    ENDIF.
    me->actual_element = create_json_element( type ).
    APPEND me->actual_element TO me->elements.

  ENDMETHOD.

  METHOD initialize_json_elements.
    CREATE DATA json_element-children TYPE t_json_element.
  ENDMETHOD.

  METHOD append_json_element.

    FIELD-SYMBOLS: <actual_element> TYPE json_element,
                   <json_element>   TYPE json_element,
                   <children>       TYPE t_json_element.

    IF has_actual_element( type = type ) EQ abap_false.
      RETURN.
    ENDIF.
    ASSIGN: me->actual_element->* TO <actual_element>,
            <actual_element>-children->* TO <children>.
    APPEND INITIAL LINE TO <children> ASSIGNING <json_element>.
    <json_element>-type = type.
    me->initialize_json_elements( CHANGING json_element = <json_element> ).
    GET REFERENCE OF <json_element> INTO me->actual_element.
    APPEND me->actual_element TO me->elements.

  ENDMETHOD.

  METHOD change_json_element_type.
    FIELD-SYMBOLS: <actual_element> TYPE json_element.
    IF me->actual_element IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN: me->actual_element->* TO <actual_element>.
    <actual_element>-type = type.
  ENDMETHOD.


  METHOD pop_json_element.
    DATA lv_lines TYPE i.

    IF lines( me->elements ) <= 1.
      RETURN.
    ENDIF.

    DATA: last_element_index TYPE i.

    last_element_index = lines( me->elements ) - 1.
    READ TABLE me->elements INTO me->actual_element INDEX last_element_index.
    lv_lines = lines( me->elements ).
    DELETE me->elements INDEX lv_lines.

  ENDMETHOD.


  METHOD get_actual_json_element.
    FIELD-SYMBOLS: <actual_element> TYPE json_element.
    IF me->actual_element IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN: me->actual_element->* TO <actual_element>.
    result = <actual_element>.
  ENDMETHOD.

  METHOD get_actual_json_element_type.
    FIELD-SYMBOLS: <actual_element> TYPE json_element.
    IF me->actual_element IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN: me->actual_element->* TO <actual_element>.
    result = <actual_element>-type.
  ENDMETHOD.

  METHOD in_string_esc.

    CASE character.
      WHEN 'b' OR
           'f' OR
           'n' OR
           'r' OR
           't' OR
           '\' OR
           '/' OR
           '"'.
        me->step_name = steps-in_string.
        result = scan_result-continue.
        RETURN.
    ENDCASE.

    result = me->gen_error(
             character = character
             context   = 'in string escape code' ).

  ENDMETHOD.

  METHOD process_string_value.

    DATA: value_substring TYPE string,
          result_tab      TYPE match_result_tab,
          lenght          TYPE i.

    FIELD-SYMBOLS: <result> LIKE LINE OF result_tab.

    FIND FIRST OCCURRENCE OF REGEX '"|\\' IN SECTION OFFSET me->no_characters_processed OF json RESULTS result_tab.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE result_tab ASSIGNING <result> INDEX 1.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    lenght = <result>-offset - me->no_characters_processed.

    value_substring = json+me->no_characters_processed(lenght).
    me->append_string( value_substring ).

    me->no_characters_processed = <result>-offset.

  ENDMETHOD.

ENDCLASS.
