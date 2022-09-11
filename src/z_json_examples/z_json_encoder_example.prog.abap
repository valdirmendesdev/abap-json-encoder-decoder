*&---------------------------------------------------------------------*
*& Report Z_JSON_ENCODER_EXAMPLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_json_encoder_example LINE-SIZE 600.


CLASS json_examples DEFINITION.

  PUBLIC SECTION.

    TYPES:

      BEGIN OF nested_struct,
        string_value TYPE string,
      END OF nested_struct,

      nested_table TYPE STANDARD TABLE OF nested_struct WITH DEFAULT KEY,

      BEGIN OF example_type,
        string_value          TYPE string,
        char_value            TYPE c LENGTH 1,
        int_value             TYPE i,
        simple_date           TYPE sy-datum,
        time_value            TYPE sy-uzeit,
        timestamp_value       TYPE timestamp,
        float_value           TYPE decfloat16,
        negative_float_value  TYPE decfloat16,
        conversion_exit_value TYPE matnr,
        boolean_value         TYPE abap_bool,
        old_boolean_value     TYPE boolean,
        nested_struct_value   TYPE nested_struct,
        nested_internal_table TYPE nested_table,
      END OF example_type.

    METHODS:  constructor.

    METHODS: show_examples.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: data_to_json            TYPE example_type,
          configuration_generator TYPE REF TO zcl_json_config_generator,
          json_configuration      TYPE REF TO zcl_json_element_config,
          encoder                 TYPE REF TO zcl_json_encoder_decoder,
          json_string_result      TYPE string.

    METHODS default_config_empty_struct.

    METHODS show_result_and_configurations IMPORTING example_title TYPE string.
    METHODS empty_camel_off_requi_field_on.
    METHODS generate_json_result.
    METHODS empty_camel_on_requirefield_on.
    METHODS filled_with_default_config.
    METHODS fill_struct.
    METHODS fill_nested_struct.
    METHODS fill_nested_internal_table.
    METHODS fill_all_data_structures.
    METHODS rename_simple_json_field_name.
    METHODS generate_json_config.
    METHODS rename_nested_struct_json_name.
    METHODS rename_nested_table_json_name.
    METHODS changing_field_default_setting.
    METHODS show_json_result.

ENDCLASS.

CLASS json_examples IMPLEMENTATION.


  METHOD constructor.

    "By default, configuration generator is setted:
    " - to transform variable name underscore to CamelCase (name_to_camel)
    " - to not generate fields without value in json (require_all_fields)
    configuration_generator = NEW #(
*    name_to_camel      = abap_true
*    require_all_fields = abap_false
    ).

    encoder = NEW #( ).

  ENDMETHOD.


  METHOD show_examples.

    default_config_empty_struct( ).
    empty_camel_on_requirefield_on( ).
    empty_camel_off_requi_field_on( ).
    filled_with_default_config( ).
    rename_simple_json_field_name( ).
    rename_nested_struct_json_name( ).
    rename_nested_table_json_name( ).
    changing_field_default_setting( ).

  ENDMETHOD.


  METHOD generate_json_result.
    json_string_result = encoder->encode(
      value          = data_to_json
      element_config = json_configuration
    ).
  ENDMETHOD.


  METHOD show_json_result.
    WRITE: / 'JSON:', json_string_result, /.
  ENDMETHOD.


  METHOD show_result_and_configurations.

    FORMAT INTENSIFIED ON.
    WRITE: / cl_abap_char_utilities=>horizontal_tab, '***', example_title,'***', /.
    FORMAT INTENSIFIED OFF.

    WRITE: / 'Configuration Generator settings:'.
    WRITE: / |{ cl_abap_char_utilities=>horizontal_tab } Name to CamelCase.: "{ configuration_generator->name_to_camel_case }"|,
           / |{ cl_abap_char_utilities=>horizontal_tab } Require all fields: "{ configuration_generator->require_all_fields }"|, /.

    show_json_result( ).
  ENDMETHOD.


  METHOD default_config_empty_struct.

    generate_json_config( ).
    generate_json_result( ).
    show_result_and_configurations( 'With default configuration' ).

  ENDMETHOD.


  METHOD empty_camel_on_requirefield_on.

    configuration_generator->name_to_camel_case = abap_true.
    configuration_generator->require_all_fields = abap_true.
    generate_json_config( ).

    generate_json_result( ).
    show_result_and_configurations( 'CamelCase -> ON and Require All Fields -> ON' ).

  ENDMETHOD.


  METHOD empty_camel_off_requi_field_on.

    configuration_generator->name_to_camel_case = abap_false.
    configuration_generator->require_all_fields = abap_true.
    generate_json_config( ).

    generate_json_result( ).
    show_result_and_configurations( 'CamelCase -> OFF and Require All Fields -> ON' ).

  ENDMETHOD.


  METHOD filled_with_default_config.

    "Default config!
    configuration_generator->name_to_camel_case = abap_true.
    configuration_generator->require_all_fields = abap_false.
    generate_json_config( ).

    fill_all_data_structures( ).
    fill_struct( ).
    generate_json_result( ).
    show_result_and_configurations( 'Filled structure with default configuration' ).

  ENDMETHOD.


  METHOD fill_all_data_structures.
    fill_struct( ).
    fill_nested_struct( ).
    fill_nested_internal_table( ).
  ENDMETHOD.


  METHOD fill_struct.
    data_to_json-string_value                       = 'String value'.
    data_to_json-char_value                         = 'C'.
    data_to_json-int_value                          = 256.
    data_to_json-simple_date                        = '20220911'.
    data_to_json-time_value                         = '161609'.
    data_to_json-timestamp_value                    = '20220911161609'.
    data_to_json-float_value                        = '10.020304'.
    data_to_json-negative_float_value               = '10.020304-'.
    data_to_json-conversion_exit_value              = '000000000000000018'.
    data_to_json-boolean_value                      = abap_true.
    data_to_json-old_boolean_value                  = 'X'.
  ENDMETHOD.


  METHOD fill_nested_struct.
    data_to_json-nested_struct_value-string_value   = 'Nested string value'.
  ENDMETHOD.


  METHOD fill_nested_internal_table.

    DATA: struct TYPE nested_struct.

    struct-string_value = 'internal table value'.
    APPEND struct TO data_to_json-nested_internal_table.
  ENDMETHOD.


  METHOD rename_simple_json_field_name.

    generate_json_config( ).
    json_configuration->change_ext_name_by_path( abap_name = 'STRING_VALUE' ext_name = 'MY_cUsTOM_fieldName' ).

    CLEAR: data_to_json.
    data_to_json-string_value = 'Field with custom name'.
    generate_json_result( ).

    show_result_and_configurations( 'Renaming JSON a field name' ).

  ENDMETHOD.


  METHOD generate_json_config.
    json_configuration = configuration_generator->generate_data_type_config( data_to_json ).
  ENDMETHOD.


  METHOD rename_nested_struct_json_name.

    generate_json_config( ).
    json_configuration->change_ext_name_by_path( abap_name = 'NESTED_STRUCT_VALUE.STRING_VALUE' ext_name = 'MY_cUsTOM_fieldName' ).

    CLEAR: data_to_json.
    data_to_json-nested_struct_value-string_value = 'Nested struct with JSON name customized'.
    generate_json_result( ).

    show_result_and_configurations( 'Renaming JSON a field name of nested struct' ).

  ENDMETHOD.


  METHOD rename_nested_table_json_name.

    generate_json_config( ).
    json_configuration->change_ext_name_by_path( abap_name = 'NESTED_INTERNAL_TABLE' ext_name = 'myArrayName' ).
    json_configuration->change_ext_name_by_path( abap_name = 'NESTED_INTERNAL_TABLE.TLINE.STRING_VALUE' ext_name = 'customizadedField_in_table_line' ).

    CLEAR: data_to_json.
    fill_nested_internal_table( ).
    generate_json_result( ).

    show_result_and_configurations( 'Renaming JSON field name of a internal table structure' ).

  ENDMETHOD.


  METHOD changing_field_default_setting.

    generate_json_config( ).

    CLEAR: data_to_json.
    data_to_json-conversion_exit_value = '000000000000000018'.

    generate_json_result( ).

    show_result_and_configurations( 'Changing a field settings - Default field settings' ).

    DATA(field_config) = json_configuration->get_child_by_abap_name( 'CONVERSION_EXIT_VALUE' ).
    field_config->use_conversion_exit = abap_false.

    field_config = json_configuration->get_element_by_path( 'NESTED_STRUCT_VALUE.STRING_VALUE' ).
    field_config->ext_name = 'customizedname'.
    field_config->required = abap_true.

    generate_json_result( ).

    WRITE: / 'Changed field settings - Conversion Exit -> OFF'.
    show_json_result( ).

  ENDMETHOD.


ENDCLASS.


START-OF-SELECTION.

  DATA(examples) = NEW json_examples( ).
  examples->show_examples( ).
