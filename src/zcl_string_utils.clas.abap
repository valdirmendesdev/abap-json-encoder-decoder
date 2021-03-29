CLASS zcl_string_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS: abap.

    METHODS:
      to_lower_camel
        IMPORTING input         TYPE csequence
        RETURNING VALUE(result) TYPE string,
      to_lower_case
        IMPORTING input         TYPE csequence
        RETURNING VALUE(result) TYPE string,
      remove_accented_chars
        IMPORTING input         TYPE csequence
        RETURNING VALUE(result) TYPE string,

      if_is_not_initial
        IMPORTING input         TYPE any
        RETURNING VALUE(result) TYPE string,

      if_value
        IMPORTING input         TYPE any
        RETURNING VALUE(result) TYPE string,

      remove_special_characters
        IMPORTING input         TYPE clike
        RETURNING VALUE(result) TYPE string,

      trim
        IMPORTING input         TYPE clike
        RETURNING VALUE(result) TYPE string,

      split_in_table
        IMPORTING input         TYPE clike
                  separator     TYPE clike
        RETURNING VALUE(result) TYPE string_t,

      to_xstring IMPORTING input         TYPE string
                           encoding      TYPE abap_encoding OPTIONAL
                 RETURNING VALUE(result) TYPE xstring,

      to_binary IMPORTING input      TYPE string
                EXPORTING binary_tab TYPE solix_tab
                          size       TYPE i,
      replace_characters
        IMPORTING input         TYPE clike
                  with_char     TYPE clike DEFAULT ''
        RETURNING VALUE(result) TYPE string.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_string_utils IMPLEMENTATION.


  METHOD if_is_not_initial.

    IF input IS INITIAL.
      RETURN.
    ENDIF.

    result = input.

  ENDMETHOD.


  METHOD if_value.

    IF input CO '0 ' OR input IS INITIAL.
      RETURN.
    ENDIF.

    result = input.

  ENDMETHOD.


  METHOD remove_accented_chars.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = input
*       intext_lg         = 0
*       inter_cp          = '0000'
*       inter_base_cp     = '0000'
*       in_cp             = '0000'
*       replacement       = 46
      IMPORTING
        outtext           = result
*       outused           =
*       outoverflow       =
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      result = input.
    ENDIF.

  ENDMETHOD.


  METHOD remove_special_characters.

    DATA: input_string    TYPE fist-searchw,
          output_string   TYPE fist-searchw,
          specharsear(50) VALUE ''' < > ! " & / = ? : ; , . - ( ) # # % ^ $ | ~ @ '.

    IF input IS INITIAL.
      RETURN.
    ENDIF.

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


  METHOD replace_characters.

    result = input.
    REPLACE ALL OCCURRENCES OF REGEX '[^\d]' IN result WITH with_char.
  ENDMETHOD.


  METHOD split_in_table.
    IF input IS INITIAL.
      RETURN.
    ENDIF.

    IF separator IS INITIAL.
      SPLIT input AT space INTO TABLE result.
      RETURN.
    ENDIF.

    SPLIT input AT separator INTO TABLE result.

  ENDMETHOD.


  METHOD to_binary.
    DATA: input_xstring TYPE xstring.

    input_xstring = to_xstring( input ).

    IF input_xstring IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = input_xstring
      IMPORTING
        output_length = size
      TABLES
        binary_tab    = binary_tab.

  ENDMETHOD.


  METHOD to_lower_camel.

    IF input IS INITIAL.
      RETURN.
    ENDIF.

    DATA: tokens     TYPE STANDARD TABLE OF string,
          first_char TYPE c.
    FIELD-SYMBOLS: <token> LIKE LINE OF tokens.

    result = to_lower_case( input ).

    TRANSLATE result:
        USING ' _/_:_~_._-_'.
    SPLIT result AT '_' INTO TABLE tokens.
    DELETE tokens WHERE table_line IS INITIAL.
    LOOP AT tokens ASSIGNING <token> FROM 2.
      first_char = <token>(1).
      TRANSLATE first_char TO UPPER CASE.
      CONCATENATE first_char <token>+1 INTO <token>.
    ENDLOOP.

    CONCATENATE LINES OF tokens INTO result.
  ENDMETHOD.


  METHOD to_lower_case.
    result = input.
    TRANSLATE result TO LOWER CASE.
  ENDMETHOD.


  METHOD to_xstring.

    IF input IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text     = input
        encoding = encoding
      IMPORTING
        buffer   = result
      EXCEPTIONS
        failed   = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.


  METHOD trim.
    result = input.
    CONDENSE result.
  ENDMETHOD.
ENDCLASS.
