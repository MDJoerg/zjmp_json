class ZCL_JMP_JSON_DATA definition
  public
  inheriting from ZCL_JMP_JSON
  create public .

public section.
*"* public components of class ZCL_JMP_JSON_DATA
*"* do not include other source files here!!!

  type-pools ABAP .
  methods CREATE_BOOLEAN
    importing
      !IV_DATA type ABAP_BOOL .
  methods CREATE_DOUBLE
    importing
      !IV_DATA type F .
  methods CREATE_BIGINT
    importing
      !IV_DATA type DATA .
  methods CREATE_INTEGER
    importing
      !IV_DATA type I .
  methods CREATE_NULL .
  methods CREATE_STRING
    importing
      !IV_DATA type STRING .
  methods GET_BOOLEAN
    returning
      value(EV_DATA) type ABAP_BOOL .
  methods GET_DATA
    returning
      value(ER_DATA) type ref to DATA .
  methods GET_DOUBLE
    returning
      value(EV_DATA) type ZJMP_JSON_DOUBLE .
  methods GET_BIGINT
    returning
      value(EV_DATA) type ZJMP_JSON_BIGINT .
  methods GET_INTEGER
    returning
      value(EV_DATA) type I .
  methods GET_STRING
    returning
      value(EV_DATA) type STRING .

  methods ZIF_JMP_JSON~GET_DATA
    redefinition .
  methods ZIF_JMP_JSON~GET_DATA_TYPE
    redefinition .
  methods ZIF_JMP_JSON~IS_NULL
    redefinition .
  methods ZIF_JMP_JSON~TO_STRING
    redefinition .
protected section.
*"* protected components of class ZCL_JMP_JSON_DATA
*"* do not include other source files here!!!

  data DATA type ref to DATA .
private section.
*"* private components of class ZCL_JMP_JSON_DATA
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_JMP_JSON_DATA IMPLEMENTATION.


method ZIF_JMP_JSON~GET_DATA.
  er_data = me->data.
endmethod.


method ZIF_JMP_JSON~GET_DATA_TYPE.
  ev_type = me->Zif_jmp_json~data_type.
endmethod.


METHOD Zif_jmp_json~is_null.
  IF me->data IS INITIAL
    OR Zif_jmp_json~data_type EQ Zcl_jmp_json=>json_null.
    ev_null = abap_true.
  ELSE.
    ev_null = abap_false.
  ENDIF.
ENDMETHOD.


METHOD Zif_jmp_json~to_string.

  DATA: lv_temp(200).
  FIELD-SYMBOLS: <data> TYPE data.
  DATA: lr_json TYPE REF TO Zcl_jmp_json.
  DATA: lv_string TYPE string.
  DATA: lv_prefix TYPE string.
  DATA: lv_suffix TYPE string.

* --- set null
  ev_string = 'null'.

* --- check for data
  IF me->data IS INITIAL.
    EXIT.
  ENDIF.

* --- check for assigned data
  ASSIGN me->data->* TO <data>.
  IF <data> IS NOT ASSIGNED.
    EXIT.
  ENDIF.

* --- prepare
  lr_json = Zcl_jmp_json=>get_instance( ).

* --- get data string
  CASE me->Zif_jmp_json~data_type.
    WHEN Zcl_jmp_json=>json_string.

      IF iv_enclosing EQ abap_true.
        lv_prefix = '"'.
        lv_suffix = '"'.
      ELSE.
        lv_prefix = ''.
        lv_suffix = ''.
      ENDIF.

      lv_string = ''.
      IF <data> IS NOT INITIAL.
        lv_string = <data>.
        lv_string = lr_json->escape_string( lv_string ).
      ENDIF.
      CONCATENATE lv_prefix
                  lv_string
                  lv_suffix
                  INTO  ev_string.
    WHEN Zcl_jmp_json=>json_integer.
      WRITE <data> TO lv_temp.
      ev_string = lv_temp.
      REPLACE ALL OCCURRENCES OF '.' IN ev_string WITH ''.
      CONDENSE ev_string.
    WHEN Zcl_jmp_json=>json_double.
      WRITE <data> TO lv_temp EXPONENT 0 DECIMALS decimals.
      CONDENSE lv_temp.
      ev_string = lv_temp.
      REPLACE ALL OCCURRENCES OF ',' IN ev_string WITH '.'.
    WHEN Zcl_jmp_json=>json_boolean.
      IF <data> EQ abap_true.
        ev_string = 'true'.
      ELSE.
        ev_string = 'false'.
      ENDIF.
    WHEN OTHERS.
      " unknown type
      ev_string = '"unknown_type"'.
  ENDCASE.



ENDMETHOD.


METHOD create_bigint.
  DATA: lv_bigint TYPE Zjmp_json_bigint.
  FIELD-SYMBOLS: <data> TYPE data.
  lv_bigint = iv_data.
  me->Zif_jmp_json~data_type = 'I'.
  CREATE DATA me->data TYPE Zjmp_json_bigint.
  ASSIGN me->data->* TO <data>.
  <data> = lv_bigint.
ENDMETHOD.


method CREATE_BOOLEAN.
  FIELD-SYMBOLS: <data> TYPE data.
  me->ZIF_JMP_json~data_type = 'B'.
  CREATE DATA me->data TYPE xfeld.
  ASSIGN me->data->* TO <data>.
  <data> = iv_data.
endmethod.


method CREATE_DOUBLE.
  FIELD-SYMBOLS: <data> TYPE data.
  me->ZIF_JMP_json~data_type = 'D'.
  CREATE DATA me->data TYPE f.
  ASSIGN me->data->* TO <data>.
  <data> = iv_data.
endmethod.


method CREATE_INTEGER.
  FIELD-SYMBOLS: <data> TYPE data.
  me->ZIF_JMP_json~data_type = 'I'.
  CREATE DATA me->data TYPE i.
  ASSIGN me->data->* TO <data>.
  <data> = iv_data.
endmethod.


method CREATE_NULL.
  me->ZIF_JMP_json~data_type = ZCL_JMP_JSON=>JSON_NULL.
  CLEAR me->data.
endmethod.


method CREATE_STRING.
  FIELD-SYMBOLS: <data> TYPE data.
  me->ZIF_JMP_json~data_type = 'S'.
  CREATE DATA me->data TYPE string.
  ASSIGN me->data->* TO <data>.
  <data> = iv_data.
endmethod.


METHOD get_bigint.
  FIELD-SYMBOLS: <data> TYPE data.
  IF me->data IS NOT INITIAL.
    ASSIGN me->data->* TO <data>.
    IF <data> IS ASSIGNED.
      ev_data = <data>.
    ENDIF.
  ENDIF.
ENDMETHOD.


method GET_BOOLEAN.
  FIELD-SYMBOLS: <data> TYPE data.
  IF me->data IS NOT INITIAL.
    ASSIGN me->data->* TO <data>.
    IF <data> IS ASSIGNED.
      ev_data = <data>.
    ENDIF.
  ENDIF.
endmethod.


method GET_DATA.
  er_data = me->data.
endmethod.


method GET_DOUBLE.
  FIELD-SYMBOLS: <data> TYPE data.
  IF me->data IS NOT INITIAL.
    ASSIGN me->data->* TO <data>.
    IF <data> IS ASSIGNED.
      ev_data = <data>.
    ENDIF.
  ENDIF.
endmethod.


method GET_INTEGER.
  FIELD-SYMBOLS: <data> TYPE data.
  IF me->data IS NOT INITIAL.
    ASSIGN me->data->* TO <data>.
    IF <data> IS ASSIGNED.
      ev_data = <data>.
    ENDIF.
  ENDIF.
endmethod.


method GET_STRING.
  FIELD-SYMBOLS: <data> TYPE data.
  IF me->data IS NOT INITIAL.
    ASSIGN me->data->* TO <data>.
    IF <data> IS ASSIGNED.
      ev_data = <data>.
    ENDIF.
  ENDIF.
endmethod.
ENDCLASS.
