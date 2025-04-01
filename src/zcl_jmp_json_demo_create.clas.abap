CLASS zcl_jmp_json_demo_create DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_jmp_json_demo_create IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

* -------- local data
    DATA lv_double   TYPE zjmp_json_double.
    DATA lv_data     TYPE string.




* -------- 0) init
  zcl_jmp_json=>set_decimals( 5 ).
  lv_double = '1234.876432'.
  lv_data   = 'my litte string with chars to "escape" and special chars äöß'.
  CONCATENATE lv_data '-' sy-uname INTO lv_data SEPARATED BY ' '.

* -------- 1) get the handler
  data(lo_json) = zcl_jmp_json=>get_instance( ).

* -------- 2) get a new json object as value holder
  data(lo_output) = lo_json->new_object( ).


* -------- 3) add some data to the main json object
  lo_output->put_string( iv_id = 'string' iv_data = lv_data ).
  lo_output->put_boolean( iv_id = 'boolean' iv_data = abap_true ).
  lo_output->put_integer( iv_id = 'integer' iv_data = 45 ).
  lo_output->put_double( iv_id = 'double' iv_data = lv_double ).

* -------- 4) create an array and fill it with integer values
  data(lo_array) = lo_json->new_array( ).
  DO 20 TIMES.
    lo_array->put_integer( sy-index ).
  ENDDO.
  lo_output->put( iv_id = 'array_integer' ir_data = lo_array ).


* -------- 5) create a subobject with an integer array
*             and add it to the main output object
  data(lo_object) = lo_json->new_object( ).
  lo_array = lo_json->new_array( ).
  lo_array->put_string( 'one' ).
  lo_array->put_string( 'two' ).
  lo_array->put_string( 'three' ).
  lo_object->put( iv_id = 'array_string' ir_data = lo_array ).
  lo_object->put_string( iv_id = 'some_id' iv_data = 'other_value' ).

  lo_output->put( iv_id = 'subobject' ir_data = lo_object ).

* -------- 6) get the json output as string
  data(lv_output) = lo_output->zif_jmp_json~to_string( ).
  out->write( lv_output ).

* -------- 7) get the content of the output object as keys
  data(lo_names)    = lo_output->names( ).
  data(lv_len)      = lo_names->length( ).
  data(lv_index)    = 0.

  DO lv_len TIMES.
    data(lo_data)    = lo_names->get( lv_index ).
    data(lv_type)    = lo_data->get_data_type( ).
    data(lv_is_null) = lo_data->is_null( ).
    data(lv_value)   = lo_data->to_string( ).

    out->write( |{ lv_index } - { lv_type } - { lv_is_null } - { lv_data }| ).
    ADD 1 TO lv_index.
  ENDDO.


* ===================== parse string to json
  data lo_input type ref to zcl_jmp_json_object.
  .
  data(lo_parsed) = lo_json->parse( lv_output ).
  IF lo_parsed IS NOT INITIAL.
    lo_input ?= lo_parsed.
* use the json object now
    lv_data = lo_input->get_string( 'string' ).
    out->WRITE( lv_data ).
  ENDIF.


  ENDMETHOD.
ENDCLASS.
