class ZCL_JMP_JSON_OBJECT definition
  public
  inheriting from ZCL_JMP_JSON
  create public .

public section.
*"* public components of class ZCL_JMP_JSON_OBJECT
*"* do not include other source files here!!!

  methods PUT_STRING
    importing
      !IV_ID type ZJMP_JSON_ID
      !IV_DATA type STRING optional .
  methods PUT_TEXT
    importing
      !IV_ID type ZJMP_JSON_ID
      !IV_DATA type DATA optional .
  methods PUT_OBJECT
    importing
      !IV_ID type ZJMP_JSON_ID
      !IR_DATA type ref to ZCL_JMP_JSON_OBJECT .
  methods PUT_NULL
    importing
      !IV_ID type ZJMP_JSON_ID .
  methods PUT_INTEGER
    importing
      !IV_ID type ZJMP_JSON_ID
      !IV_DATA type I optional .
  methods PUT_DOUBLE
    importing
      !IV_ID type ZJMP_JSON_ID
      !IV_DATA type ZJMP_JSON_DOUBLE optional .
  type-pools ABAP .
  methods PUT_BOOLEAN
    importing
      !IV_ID type ZJMP_JSON_ID
      !IV_DATA type ABAP_BOOL optional .
  methods PUT_ARRAY
    importing
      !IV_ID type ZJMP_JSON_ID
      !IR_DATA type ref to ZCL_JMP_JSON_ARRAY .
  methods PUT
    importing
      !IV_ID type ZJMP_JSON_ID
      !IR_DATA type ref to ZIF_JMP_JSON .
  methods NAMES
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_ARRAY .
  methods LENGTH
    returning
      value(EV_COUNT) type I .
  methods GET_STRING
    importing
      !IV_ID type ZJMP_JSON_ID
    returning
      value(EV_DATA) type STRING .
  methods GET_PATH
    importing
      !IV_PATH type STRING
    returning
      value(ER_JSON) type ref to ZIF_JMP_JSON .
  methods GET_OBJECT
    importing
      !IV_ID type ZJMP_JSON_ID
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_OBJECT .
  methods GET_INTEGER
    importing
      !IV_ID type ZJMP_JSON_ID
    returning
      value(EV_DATA) type I .
  methods GET_ID_FOR_UPPERCASE
    importing
      !IV_ID type ANY
    returning
      value(EV_ID) type STRING .
  methods GET_DOUBLE
    importing
      !IV_ID type ZJMP_JSON_ID
    returning
      value(EV_DATA) type ZJMP_JSON_DOUBLE .
  methods GET_BOOLEAN
    importing
      !IV_ID type ZJMP_JSON_ID
    returning
      value(EV_DATA) type ABAP_BOOL .
  methods GET_ARRAY
    importing
      !IV_ID type ZJMP_JSON_ID
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_ARRAY .
  methods GET
    importing
      !IV_ID type ANY
    returning
      value(ER_JSON) type ref to ZIF_JMP_JSON .

  methods ZIF_JMP_JSON~GET_DATA
    redefinition .
  methods ZIF_JMP_JSON~GET_DATA_TYPE
    redefinition .
  methods ZIF_JMP_JSON~IS_NULL
    redefinition .
  methods ZIF_JMP_JSON~TO_STRING
    redefinition .
protected section.
*"* protected components of class ZCL_JMP_JSON_OBJECT
*"* do not include other source files here!!!

  data DATA type ZJMP_JSON_DATA_OBJECT_T .
private section.
*"* private components of class ZCL_JMP_JSON_OBJECT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_JMP_JSON_OBJECT IMPLEMENTATION.


method ZIF_JMP_JSON~GET_DATA.
  get REFERENCE OF me->data into er_data.
endmethod.


METHOD Zif_jmp_json~get_data_type.
  ev_type = Zcl_jmp_json=>json_object.
ENDMETHOD.


method ZIF_JMP_JSON~IS_NULL.
  ev_null = abap_false.
endmethod.


METHOD Zif_jmp_json~to_string.

  DATA: ls_data TYPE Zjmp_json_data_object_s.
  DATA: lv_data TYPE string.
  DATA: lv_line TYPE string.
  DATA: lv_value TYPE string.
  DATA: lr_value TYPE REF TO Zif_jmp_json.

  LOOP AT me->data INTO ls_data.
    IF ls_data-data IS NOT INITIAL.
      lr_value ?= ls_data-data.
      lv_value = lr_value->to_string( ).

      CONCATENATE '"'
                  ls_data-id
                  '":'
                  lv_value
                  INTO lv_line.

      IF lv_data IS INITIAL.
        lv_data = lv_line.
      ELSE.
        CONCATENATE lv_data
                    ','
                    lv_line
                    INTO lv_data.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CONCATENATE '{'
              lv_data
              '}'
              INTO ev_string.

ENDMETHOD.


method GET.
  DATA: ls_data LIKE LINE OF me->data.
  data: lv_id TYPE ZJMP_JSON_ID.

  lv_id = iv_id.

  READ TABLE me->data INTO ls_data
    WITH KEY id = lv_id.
  IF sy-subrc EQ 0.
    er_json = ls_data-data.
  ENDIF.

endmethod.


method GET_ARRAY.
  DATA: ls_data LIKE LINE OF me->data.

  READ TABLE me->data INTO ls_data
    WITH KEY id = iv_id.
  IF sy-subrc EQ 0.
    er_json ?= ls_data-data.
  ENDIF.

endmethod.


method GET_BOOLEAN.
* ------- local data
  FIELD-SYMBOLS: <data> TYPE data.
  DATA: lr_data TYPE REF TO data.
  DATA: lr_json TYPE REF TO ZIF_JMP_json.

* ------- get data
  lr_json = me->get( iv_id ).
  IF lr_json IS NOT INITIAL.
    lr_data = lr_json->get_data( ).
    IF lr_data IS NOT INITIAL.
      ASSIGN lr_data->* TO <data>.
      IF <data> IS ASSIGNED.
        ev_data = <data>.
      ENDIF.
    ENDIF.
  ENDIF.
endmethod.


method GET_DOUBLE.
* ------- local data
  FIELD-SYMBOLS: <data> TYPE data.
  DATA: lr_data TYPE REF TO data.
  DATA: lr_json TYPE REF TO ZIF_JMP_json.

* ------- get data
  lr_json = me->get( iv_id ).
  IF lr_json IS NOT INITIAL.
    lr_data = lr_json->get_data( ).
    IF lr_data IS NOT INITIAL.
      ASSIGN lr_data->* TO <data>.
      IF <data> IS ASSIGNED.
        ev_data = <data>.
      ENDIF.
    ENDIF.
  ENDIF.
endmethod.


method GET_ID_FOR_UPPERCASE.

  DATA: ls_data LIKE LINE OF me->data.
  DATA: lv_id TYPE string.
  DATA: lv_check TYPE string.

  CLEAR ev_id.
  lv_id = iv_id.
  TRANSLATE lv_id TO UPPER CASE.

  LOOP AT me->data INTO ls_data.
    lv_check = ls_data-id.
    TRANSLATE lv_check TO UPPER CASE.

    IF lv_check = lv_id.
      ev_id = ls_data-id.
      EXIT.
    ENDIF.
  ENDLOOP.

endmethod.


method GET_INTEGER.
* ------- local data
  FIELD-SYMBOLS: <data> TYPE data.
  DATA: lr_data TYPE REF TO data.
  DATA: lr_json TYPE REF TO ZIF_JMP_json.

* ------- get data
  lr_json = me->get( iv_id ).
  IF lr_json IS NOT INITIAL.
    lr_data = lr_json->get_data( ).
    IF lr_data IS NOT INITIAL.
      ASSIGN lr_data->* TO <data>.
      IF <data> IS ASSIGNED.
        ev_data = <data>.
      ENDIF.
    ENDIF.
  ENDIF.
endmethod.


method GET_OBJECT.
  DATA: ls_data LIKE LINE OF me->data.

  READ TABLE me->data INTO ls_data
    WITH KEY id = iv_id.
  IF sy-subrc EQ 0.
    er_json ?= ls_data-data.
  ENDIF.

endmethod.


method GET_PATH.

* ----- local data
  DATA: lr_cur TYPE REF TO ZCL_JMP_json_object.
  DATA: lr_new TYPE REF TO ZCL_JMP_json_object.
  DATA: BEGIN OF ls_part,
          part(50),
        END OF ls_part.
  DATA: lt_part LIKE TABLE OF ls_part.
  DATA: lv_lin TYPE i.
  DATA: lv_max TYPE i.

* ----- check
  CHECK iv_path IS NOT INITIAL.

* ----- init
  lr_cur ?= me.
  SPLIT iv_path AT '/' INTO TABLE lt_part.
  DESCRIBE TABLE lt_part LINES lv_lin.
  CHECK lv_lin GT 0.

* ----- loop
  lv_max = lv_lin - 1.
  DO lv_max TIMES.
    READ TABLE lt_part INTO ls_part INDEX sy-index.
    IF lr_cur IS INITIAL.
      EXIT.
    ENDIF.

    IF ls_part-part IS INITIAL.
      " do nothing
    ELSE.
      lr_cur ?= lr_cur->get( ls_part-part ).
    ENDIF.
  ENDDO.

* ----- get last
  READ TABLE lt_part INTO ls_part INDEX lv_lin.
  IF ls_part-part IS INITIAL.
    er_json ?= lr_cur.
  ELSE.
    IF lr_cur IS NOT INITIAL.
      er_json = lr_cur->get( ls_part-part ).
    ENDIF.
  ENDIF.

endmethod.


method GET_STRING.
* ------- local data
  FIELD-SYMBOLS: <data> TYPE data.
  DATA: lr_data TYPE REF TO data.
  DATA: lr_json TYPE REF TO ZIF_JMP_json.

* ------- get data
  lr_json = me->get( iv_id ).
  IF lr_json IS NOT INITIAL.
    lr_data = lr_json->get_data( ).
    IF lr_data IS NOT INITIAL.
      ASSIGN lr_data->* TO <data>.
      IF <data> IS ASSIGNED.
        ev_data = <data>.
      ENDIF.
    ENDIF.
  ENDIF.
endmethod.


method LENGTH.
  DESCRIBE table me->data lines ev_count.
endmethod.


method NAMES.
  DATA: lr_json TYPE REF TO ZCL_JMP_json.
  DATA: ls_data LIKE LINE OF me->data.
  DATA: lr_data TYPE REF TO ZIF_JMP_json.

  lr_json = ZCL_JMP_json=>get_instance( ).
  er_json ?= lr_json->new_array( ).

  LOOP AT me->data INTO ls_data.
    lr_data = lr_json->new_string( ls_data-id ).
    er_json->put( lr_data ).
  ENDLOOP.
endmethod.


method PUT.
  DATA: ls_data LIKE LINE OF me->data.

  ls_data-id = iv_id.
  ls_data-data ?= ir_data.

  READ TABLE me->data TRANSPORTING NO FIELDS
    WITH KEY id = ls_data-id.
  IF sy-subrc EQ 0.
    DELETE me->data WHERE id = ls_data-id.
  ENDIF.

  INSERT ls_data INTO TABLE me->data.
endmethod.


METHOD put_array.

  me->put( iv_id = iv_id ir_data = ir_data ).

*  DATA: ls_data LIKE LINE OF me->data.
*
*  ls_data-id = iv_id.
*  ls_data-data ?= ir_data.
*
*  INSERT ls_data INTO TABLE me->data.

ENDMETHOD.


method PUT_BOOLEAN.

  DATA: lr_json TYPE REF TO ZCL_JMP_json.
  DATA: lr_data TYPE REF TO ZIF_JMP_json.

  lr_json = ZCL_JMP_json=>get_instance( ).
  lr_data = lr_json->NEW_BOOLEAN( iv_data ).
  me->put( iv_id = iv_id ir_data = lr_data ).

endmethod.


method PUT_DOUBLE.

  DATA: lr_json TYPE REF TO ZCL_JMP_json.
  DATA: lr_data TYPE REF TO ZIF_JMP_json.

  lr_json = ZCL_JMP_json=>get_instance( ).
  lr_data = lr_json->NEW_DOUBLE( iv_data ).
  me->put( iv_id = iv_id ir_data = lr_data ).

endmethod.


method PUT_INTEGER.

  DATA: lr_json TYPE REF TO ZCL_JMP_json.
  DATA: lr_data TYPE REF TO ZIF_JMP_json.

  lr_json = ZCL_JMP_json=>get_instance( ).
  lr_data = lr_json->NEW_INTEGER( iv_data ).
  me->put( iv_id = iv_id ir_data = lr_data ).

endmethod.


method PUT_NULL.

  DATA: lr_json TYPE REF TO ZCL_JMP_json.
  DATA: lr_data TYPE REF TO ZIF_JMP_json.

  lr_json = ZCL_JMP_json=>get_instance( ).
  lr_data = lr_json->NEW_NULL( ).
  me->put( iv_id = iv_id ir_data = lr_data ).

endmethod.


METHOD put_object.

  me->put( iv_id = iv_id ir_data = ir_data ).

*  DATA: ls_data LIKE LINE OF me->data.
*
*  ls_data-id = iv_id.
*  ls_data-data ?= ir_data.
*
*  INSERT ls_data INTO TABLE me->data.

ENDMETHOD.


method PUT_STRING.

  DATA: lr_json TYPE REF TO ZCL_JMP_json.
  DATA: lr_data TYPE REF TO ZIF_JMP_json.

  lr_json = ZCL_JMP_json=>get_instance( ).
  lr_data = lr_json->new_string( iv_data ).
  me->put( iv_id = iv_id ir_data = lr_data ).

endmethod.


METHOD put_text.

  DATA: lv_string TYPE string.

  lv_string = iv_data.

  CALL METHOD me->put_string
    EXPORTING
      iv_id   = iv_id
      iv_data = lv_string.


ENDMETHOD.
ENDCLASS.
