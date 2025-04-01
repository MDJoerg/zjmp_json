*"* components of interface ZIF_JMP_JSON
interface ZIF_JMP_JSON
  public .


  data DATA_TYPE type ZJMP_JSON_DATA_TYPE .

  methods GET_DATA_TYPE
    returning
      value(EV_TYPE) type ZJMP_JSON_DATA_TYPE .
  type-pools ABAP .
  methods TO_STRING
    importing
      !IV_ENCLOSING type ABAP_BOOL default ABAP_TRUE
    returning
      value(EV_STRING) type STRING .
  methods IS_NULL
    returning
      value(EV_NULL) type ABAP_BOOL .
  methods GET_DATA
    returning
      value(ER_DATA) type ref to DATA .
endinterface.
