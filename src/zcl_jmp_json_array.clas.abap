class ZCL_JMP_JSON_ARRAY definition
  public
  inheriting from ZCL_JMP_JSON
  create public .

public section.
*"* public components of class ZCL_JMP_JSON_ARRAY
*"* do not include other source files here!!!

  type-pools ABAP .
  methods HAS_NEXT
    returning
      value(EV_NEXT) type ABAP_BOOL .
  methods IS_EMPTY
    returning
      value(EV_EMPTY) type ABAP_BOOL .
  methods NEXT_RESET .
  methods PUT_STRING
    importing
      !IV_DATA type STRING optional .
  methods PUT_OBJECT
    importing
      !IR_DATA type ref to ZCL_JMP_JSON_OBJECT .
  methods PUT_NULL .
  methods PUT_INTEGER
    importing
      !IV_DATA type I optional .
  methods PUT_INDEX
    importing
      !IR_DATA type ref to ZIF_JMP_JSON
      !IV_INDEX type SYTABIX .
  methods PUT_DOUBLE
    importing
      !IV_DATA type ZJMP_JSON_DOUBLE optional .
  methods PUT_BOOLEAN
    importing
      !IV_DATA type ABAP_BOOL optional .
  methods PUT_ARRAY
    importing
      !IR_DATA type ref to ZCL_JMP_JSON_ARRAY .
  methods PUT
    importing
      !IR_DATA type ref to ZIF_JMP_JSON .
  methods LENGTH
    returning
      value(EV_LENGTH) type I .
  methods GET_STRING
    importing
      !IV_INDEX type I
    returning
      value(EV_DATA) type STRING .
  methods GET_OBJECT
    importing
      !IV_INDEX type I
    returning
      value(ER_DATA) type ref to ZCL_JMP_JSON_OBJECT .
  methods GET_INTEGER
    importing
      !IV_INDEX type I
    returning
      value(EV_DATA) type I .
  methods GET_DOUBLE
    importing
      !IV_INDEX type I
    returning
      value(EV_DATA) type ZJMP_JSON_DOUBLE .
  methods GET_BOOLEAN
    importing
      !IV_INDEX type I
    returning
      value(EV_DATA) type ABAP_BOOL .
  methods GET_ARRAY
    importing
      !IV_INDEX type I
    returning
      value(ER_DATA) type ref to ZCL_JMP_JSON_ARRAY .
  methods GET
    importing
      !IV_INDEX type I
    returning
      value(ER_DATA) type ref to ZIF_JMP_JSON .
  methods NEXT
    returning
      value(ER_DATA) type ref to ZIF_JMP_JSON .

  methods ZIF_JMP_JSON~GET_DATA
    redefinition .
  methods ZIF_JMP_JSON~GET_DATA_TYPE
    redefinition .
  methods ZIF_JMP_JSON~IS_NULL
    redefinition .
  methods ZIF_JMP_JSON~TO_STRING
    redefinition .
protected section.
*"* protected components of class ZCL_JMP_JSON_ARRAY
*"* do not include other source files here!!!

  data ITER_POS type I .
  data DATA type ZJMP_JSON_DATA_ARRAY_T .
private section.
*"* private components of class ZCL_JMP_JSON_ARRAY
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_JMP_JSON_ARRAY IMPLEMENTATION.


method ZIF_JMP_JSON~GET_DATA.
  get REFERENCE OF me->data into er_data.
endmethod.


METHOD Zif_jmp_json~get_data_type.
  ev_type = Zcl_jmp_json=>json_array.
ENDMETHOD.


method ZIF_JMP_JSON~IS_NULL.
  ev_null = abap_false.
endmethod.


METHOD Zif_jmp_json~to_string.

  DATA: ls_data LIKE LINE OF me->data.
  DATA: lv_data TYPE string.
  DATA: lv_value TYPE string.
  DATA: lr_value TYPE REF TO Zif_jmp_json.

  LOOP AT me->data INTO ls_data.
    IF ls_data-data IS NOT INITIAL.
      lr_value ?= ls_data-data.
      lv_value = lr_value->to_string( ).

      IF lv_data IS INITIAL.
        lv_data = lv_value.
      ELSE.
        CONCATENATE lv_data
                    ','
                    lv_value
                    INTO lv_data.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CONCATENATE '['
              lv_data
              ']'
              INTO ev_string.

ENDMETHOD.


method GET.
  DATA: ls_data LIKE LINE OF me->data.
  DATA: lv_index TYPE i.
  DATA: lv_max TYPE i.

  lv_max = me->length( ).
  lv_index = iv_index + 1.

  READ TABLE me->data INTO ls_data INDEX lv_index.
  IF sy-subrc EQ 0.
    er_data ?= ls_data-data.
  ENDIF.

endmethod.


method GET_ARRAY.
  DATA: ls_data LIKE LINE OF me->data.
  DATA: lv_index TYPE i.
  DATA: lv_max TYPE i.

  lv_max = me->length( ).
  lv_index = iv_index + 1.

  READ TABLE me->data INTO ls_data INDEX lv_index.
  IF sy-subrc EQ 0.
    er_data ?= ls_data-data.
  ENDIF.

endmethod.


method GET_BOOLEAN.
* ------- local data
  FIELD-SYMBOLS: <data> TYPE data.
  DATA: lr_data TYPE REF TO data.
  DATA: lr_json TYPE REF TO ZIF_JMP_json.

* ------- get data
  lr_json = me->get( iv_index ).
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
  lr_json = me->get( iv_index ).
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


method GET_INTEGER.
* ------- local data
  FIELD-SYMBOLS: <data> TYPE data.
  DATA: lr_data TYPE REF TO data.
  DATA: lr_json TYPE REF TO ZIF_JMP_json.

* ------- get data
  lr_json = me->get( iv_index ).
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
  DATA: lv_index TYPE i.
  DATA: lv_max TYPE i.

  lv_max = me->length( ).
  lv_index = iv_index + 1.

  READ TABLE me->data INTO ls_data INDEX lv_index.
  IF sy-subrc EQ 0.
    er_data ?= ls_data-data.
  ENDIF.

endmethod.


method GET_STRING.
* ------- local data
  FIELD-SYMBOLS: <data> TYPE data.
  DATA: lr_data TYPE REF TO data.
  DATA: lr_json TYPE REF TO ZIF_JMP_json.

* ------- get data
  lr_json = me->get( iv_index ).
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


METHOD has_next.

* ---- local data
  DATA: lv_len TYPE i.

* ---- get length
  lv_len = length( ).

* ---- check iter pos
  IF lv_len GT 0 AND iter_pos LT lv_len.
    ev_next = abap_true.
  ELSE.
    ev_next = abap_false.
  ENDIF.

ENDMETHOD.


METHOD is_empty.
  DATA: lv_len TYPE i.
  lv_len = length( ).
  IF lv_len EQ 0.
    ev_empty = abap_true.
  ELSE.
    ev_empty = abap_false.
  ENDIF.
ENDMETHOD.


method LENGTH.
  DESCRIBE TABLE me->data lines ev_length.
endmethod.


METHOD next.

* ------ get the next
  IF has_next( ) EQ abap_true.
    er_data = get( iter_pos ).
    ADD 1 TO iter_pos.
  ENDIF.

ENDMETHOD.


METHOD next_reset.
  CLEAR iter_pos.
ENDMETHOD.


method PUT.
  DATA: ls_data LIKE LINE OF me->data.

  ls_data-data ?= ir_data.
  APPEND ls_data TO me->data.

endmethod.


method PUT_ARRAY.
  DATA: ls_data LIKE LINE OF me->data.

  ls_data-data ?= ir_data.
  APPEND ls_data TO me->data.

endmethod.


method PUT_BOOLEAN.
  DATA: lr_json TYPE REF TO ZCL_JMP_json.
  DATA: lr_data TYPE REF TO ZIF_JMP_json.

  lr_json = ZCL_JMP_json=>get_instance( ).
  lr_data = lr_json->NEW_BOOLEAN( iv_data ).
  me->put( lr_data ).

endmethod.


method PUT_DOUBLE.
  DATA: lr_json TYPE REF TO ZCL_JMP_json.
  DATA: lr_data TYPE REF TO ZIF_JMP_json.

  lr_json = ZCL_JMP_json=>get_instance( ).
  lr_data = lr_json->NEW_DOUBLE( iv_data ).
  me->put( lr_data ).

endmethod.


method PUT_INDEX.
  DATA: ls_data LIKE LINE OF me->data.
  DATA: lv_index TYPE i.
  DATA: lv_max TYPE i.

  lv_max = me->length( ).
  lv_index = iv_index + 1.

  IF lv_index < 1 OR lv_index > lv_max.
    me->put( ir_data ).
  ELSE.
    ls_data-data ?= ir_data.
    INSERT ls_data INTO me->data INDEX lv_index.
  ENDIF.

endmethod.


method PUT_INTEGER.
  DATA: lr_json TYPE REF TO ZCL_JMP_json.
  DATA: lr_data TYPE REF TO ZIF_JMP_json.

  lr_json = ZCL_JMP_json=>get_instance( ).
  lr_data = lr_json->NEW_INTEGER( iv_data ).
  me->put( lr_data ).

endmethod.


method PUT_NULL.
  DATA: lr_json TYPE REF TO ZCL_JMP_json.
  DATA: lr_data TYPE REF TO ZIF_JMP_json.

  lr_json = ZCL_JMP_json=>get_instance( ).
  lr_data = lr_json->NEW_NULL( ).
  me->put( lr_data ).

endmethod.


method PUT_OBJECT.
  DATA: ls_data LIKE LINE OF me->data.

  ls_data-data ?= ir_data.
  APPEND ls_data TO me->data.

endmethod.


method PUT_STRING.
  DATA: lr_json TYPE REF TO ZCL_JMP_json.
  DATA: lr_data TYPE REF TO ZIF_JMP_json.

  lr_json = ZCL_JMP_json=>get_instance( ).
  lr_data = lr_json->new_string( iv_data ).
  me->put( lr_data ).

endmethod.
ENDCLASS.
