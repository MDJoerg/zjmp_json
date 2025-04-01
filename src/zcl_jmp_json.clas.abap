class ZCL_JMP_JSON definition
  public
  create public .

public section.
*"* public components of class ZCL_JMP_JSON
*"* do not include other source files here!!!

  interfaces ZIF_JMP_JSON .

  class-data JSON_STRING type ZJMP_JSON_DATA_TYPE value 'S'. "#EC NOTEXT .
  class-data JSON_BOOLEAN type ZJMP_JSON_DATA_TYPE value 'B'. "#EC NOTEXT .
  class-data JSON_INTEGER type ZJMP_JSON_DATA_TYPE value 'I'. "#EC NOTEXT .
  class-data JSON_DOUBLE type ZJMP_JSON_DATA_TYPE value 'D'. "#EC NOTEXT .
  class-data JSON_OBJECT type ZJMP_JSON_DATA_TYPE value 'O'. "#EC NOTEXT .
  class-data JSON_ARRAY type ZJMP_JSON_DATA_TYPE value 'A'. "#EC NOTEXT .
  class-data JSON_NULL type ZJMP_JSON_DATA_TYPE value 'N'. "#EC NOTEXT .

  methods GET_DATETIME_FROM_UNIX_INTEGER
    importing
      !IV_SECONDS_SINCE_1970 type I
    exporting
      !EV_DATE type SYDATUM
      !EV_TIME type SYUZEIT .
  class-methods SET_DECIMALS
    importing
      !IV_DECIMALS type I default 8 .
  methods GET_MAPPER
    returning
      value(ER_MAPPER) type ref to ZCL_JMP_JSON_MAPPER .
  class-methods SET_EXTENSION
    importing
      !IR_EXTENSION type ref to ZIF_JMP_JSON_EXTENSION .
  methods CLONE
    importing
      !IR_JSON type ref to ZIF_JMP_JSON
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON .
  class-methods PARSE
    importing
      !IV_JSON type ANY
    returning
      value(ER_JSON) type ref to ZIF_JMP_JSON .
  class-methods GET_INSTANCE
    returning
      value(ER_INSTANCE) type ref to ZCL_JMP_JSON .
  class-methods GET_EXTENSION
    returning
      value(ER_EXTENSION) type ref to ZIF_JMP_JSON_EXTENSION .
  methods NEW_TIMESTAMP
    importing
      !IV_DATA type TIMESTAMPL optional
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_DATA .
  methods NEW_STRING
    importing
      !IV_DATA type STRING optional
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_DATA .
  methods NEW_OBJECT
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_OBJECT .
  methods NEW_BIGINT
    importing
      !IV_DATA type DATA optional
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_DATA .
  methods NEW_INTEGER
    importing
      !IV_DATA type I optional
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_DATA .
  methods NEW_FILE
    importing
      !IV_FILE type XSTRING optional
      !IV_FILENAME type STRING optional
      !IV_FILESIZE type I optional
      !IV_MIMETYPE type STRING optional
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_DATA .
  methods NEW_DOUBLE
    importing
      !IV_DATA type ZJMP_JSON_DOUBLE optional
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_DATA .
  methods NEW_NULL
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_DATA .
  type-pools ABAP .
  methods NEW_BOOLEAN
    importing
      !IV_DATA type ABAP_BOOL optional
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_DATA .
  methods NEW_ARRAY
    returning
      value(ER_JSON) type ref to ZCL_JMP_JSON_ARRAY .
  methods GET_TIMESTAMP
    importing
      !IR_DATA type ref to ZIF_JMP_JSON optional
      !IV_DATA type STRING optional
    returning
      value(EV_TIMESTAMP) type TIMESTAMPL .
  methods GET_FILE
    importing
      !IR_DATA type ref to ZIF_JMP_JSON
    exporting
      !EV_FILE type XSTRING
      !EV_FILENAME type STRING
      !EV_MIMETYPE type STRING
      !EV_FILESIZE type I
      !EV_TEXT type STRING .
  methods ESCAPE_STRING
    importing
      !IV_UNESCAPED type STRING
    returning
      value(EV_ESCAPED) type STRING .
  methods BASE64_ENCODE
    importing
      !IV_BINARY type XSTRING
    returning
      value(EV_B64CODED) type STRING .
  methods BASE64_DECODE
    importing
      value(IV_B64CODED) type STRING
    returning
      value(EV_BINARY) type XSTRING .
protected section.
*"* protected components of class ZCL_JMP_JSON
*"* do not include other source files here!!!

  class-data DECIMALS type I value 8. "#EC NOTEXT .
  class-data EXTENSION type ref to ZIF_JMP_JSON_EXTENSION .
private section.
*"* private components of class ZCL_JMP_JSON
*"* do not include other source files here!!!

  class-data INSTANCE type ref to ZCL_JMP_JSON .
ENDCLASS.



CLASS ZCL_JMP_JSON IMPLEMENTATION.


METHOD Zif_jmp_json~get_data.
  FIELD-SYMBOLS: <data> TYPE data.
  ASSIGN ('me->data') TO <data>.
  IF <data> IS ASSIGNED.
    GET REFERENCE OF <data> INTO er_data.
  ENDIF.
ENDMETHOD.


method BASE64_DECODE.
* copied from SSFC_BASE64_ENCODE
  include_constants.
  DATA: bincopy TYPE xstring.
  DATA: binlen TYPE i.
  DATA: strleng TYPE i.


  CALL 'SSF_ABAP_SERVICE'                                 "#EC CI_CCALL
    ID 'OPCODE'          FIELD   ssf_opcodes-base64decode
    ID 'B64DATA'         FIELD   iv_b64coded
    ID 'BINDATA'         FIELD   ev_binary.

  IF sy-subrc > 0.
  ENDIF.

endmethod.


method BASE64_ENCODE.
* copied from SSFC_BASE64_ENCODE
  include_constants.
  DATA: bincopy TYPE xstring.
  DATA: binlen TYPE i.
  DATA: strleng TYPE i.

*  if binleng = 0. "convert whole xstring
*    bincopy = bindata.
*  else.           "convert substring
*    strleng = xstrlen( bindata ).
*    if binleng < 0 or binleng > strleng.
*      message e310(1s) raising ssf_krn_invalid_parlen.
*    endif.
*    bincopy = bindata(binleng).
*  endif.

  CALL 'SSF_ABAP_SERVICE'                                 "#EC CI_CCALL
    ID 'OPCODE'          FIELD   ssf_opcodes-base64encode
    ID 'BINDATA'         FIELD   iv_binary
    ID 'B64DATA'         FIELD   ev_b64coded.

  IF sy-subrc > 0.
*    case sy-subrc.
*      when ssfkrn_rc-krn_noop.
*        message e301(1s) raising ssf_krn_noop.
*      when ssfkrn_rc-krn_nomemory.
*        message e302(1s) raising ssf_krn_nomemory.
*      when ssfkrn_rc-krn_opinv.
*        message e303(1s) raising ssf_krn_opinv.
**   WHEN SSFKRN_RC-KRN_INPUT_DATA_ERROR.
**     MESSAGE E308(1S) RAISING SSF_KRN_INPUT_DATA_ERROR.
*      when ssfkrn_rc-krn_invalid_par.
*        message e309(1s) raising ssf_krn_invalid_par.
*      when others.
*        message e110(1s) raising ssf_krn_error.
*    endcase.
  ENDIF.

endmethod.


method CLONE.

* ------- local data
  DATA: lv_json TYPE string.

* ------- check
  CHECK ir_json IS NOT INITIAL.

* ------- get as string
  lv_json = ir_json->to_string( ).
  CHECK lv_json IS NOT INITIAL.

* ------- get as new object
  er_json ?= me->parse( lv_json ).

endmethod.


method ESCAPE_STRING.
  ev_escaped = cl_http_utility=>escape_javascript( iv_unescaped ).
endmethod.


METHOD get_datetime_from_unix_integer.

  DATA: lv_time(8).

  PERFORM p6_to_date_time_tz IN PROGRAM rstr0400
    IF FOUND
    USING iv_seconds_since_1970 lv_time ev_date.

  CONCATENATE
    lv_time(2)
    lv_time+3(2)
    lv_time+6(2)
    INTO ev_time.

ENDMETHOD.


method GET_EXTENSION.
  er_extension ?= extension.
endmethod.


method GET_FILE.

* ------ local data
  DATA: lv_string TYPE string.
  DATA: lv_mimetype TYPE string.
  DATA: lv_data TYPE string.
  DATA: lv_filename TYPE string.
  DATA: lv_text TYPE string.
  DATA: lv_tmp TYPE string.
  DATA: lr_string TYPE REF TO ZCL_JMP_json_data.

* ------ check data type
  CHECK ir_data IS NOT INITIAL.
  IF ir_data->get_data_type( ) NE ZCL_JMP_json=>json_string.
    EXIT.
  ENDIF.

* ----- check content
  lr_string ?= ir_data.
  lv_string = lr_string->get_string( ).
  CHECK lv_string IS NOT INITIAL.
  IF lv_string CS 'data:'.
    IF sy-fdpos = 0.
*     transform and check
      lv_string = lv_string+5.
      SPLIT lv_string AT ';' INTO lv_mimetype lv_data lv_filename lv_text.
      CHECK lv_mimetype IS NOT INITIAL
        AND lv_data IS NOT INITIAL.
*     get binary data
      IF lv_data CS 'base64,'.
        IF sy-fdpos = 0.
          SPLIT lv_data AT ',' INTO lv_tmp lv_data.
          ev_file = me->base64_decode( lv_data ).
          IF ev_file IS NOT INITIAL.
            ev_filesize = XSTRLEN( ev_file ).
            ev_filename = lv_filename.
            ev_text     = lv_text.
            ev_mimetype = lv_mimetype.
            " bugfix
            REPLACE '\/' in ev_mimetype with '/'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
endmethod.


method GET_INSTANCE.
  IF ZCL_JMP_json=>instance IS INITIAL.
    CREATE OBJECT ZCL_JMP_json=>instance.
  ENDIF.
  er_instance ?= ZCL_JMP_json=>instance.
endmethod.


method GET_MAPPER.
  CREATE OBJECT er_mapper.
  er_mapper->json ?= me.
endmethod.


method GET_TIMESTAMP.

* ------ local data
  DATA: lv_string(30).
  DATA: lr_string TYPE REF TO ZCL_JMP_json_data.

* ------ check data type
  IF ir_data IS NOT INITIAL.
    IF ir_data->get_data_type( ) NE ZCL_JMP_json=>json_string.
      EXIT.
    ELSE.
      lr_string ?= ir_data.
      lv_string = lr_string->get_string( ).
    ENDIF.
  ELSEIF iv_data IS NOT INITIAL.
    lv_string = iv_data.
  ELSE.
    EXIT. "no valid input
  ENDIF.

* ----- check content
  CHECK lv_string IS NOT INITIAL.

* ------
  "          0123456789012345678901234567890
  "check for YYYY-MM-DD HH:MM:SS (TIMEZONE)
  DATA: lv_date TYPE sydatum.
  DATA: lv_time TYPE syuzeit.
  DATA: lv_timezone TYPE string.
  DATA: lv_tz   TYPE timezone VALUE 'UTC'.
  DATA: lv_pos TYPE i.

* ------ get date part
  CHECK lv_string+4(1) EQ '-'
    AND lv_string+7(1) EQ '-'
    AND lv_string+10(1) EQ ' '.
  lv_date(4)   = lv_string(4).
  lv_date+4(2) = lv_string+5(2).
  lv_date+6(2) = lv_string+8(2).

* ------ get time part
  IF lv_string+13(1) = ':'
    AND lv_string+16(1) = ':'.
    lv_time(2)    = lv_string+11(2).
    lv_time+2(2)  = lv_string+14(2).
    lv_time+4(2)  = lv_string+17(2).
  ELSE.
    lv_time = '000000'.
  ENDIF.

* ------ check for timezone
  IF lv_string CS '('.
    lv_pos = sy-fdpos + 1.
    lv_timezone = lv_string+lv_pos.
    IF lv_timezone CS ')'.
      lv_pos = sy-fdpos.
      lv_timezone = lv_timezone(lv_pos).

*     check timezone
      SELECT SINGLE tzone FROM ttzz
        INTO lv_tz
        WHERE tzone EQ lv_timezone.
      IF sy-subrc NE 0.
        lv_tz = sy-zonlo.
        IF extension IS NOT INITIAL.
          CALL METHOD extension->map_external_timezone
            EXPORTING
              iv_timezone_ext = lv_timezone
            CHANGING
              cv_timezone_int = lv_tz.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* ------ convert timezone
  CONVERT DATE lv_date TIME lv_time
  "[DAYLIGHT SAVING TIME dst]
        INTO TIME STAMP ev_timestamp TIME ZONE lv_tz.

endmethod.


method NEW_ARRAY.
  CREATE OBJECT er_json type ZCL_JMP_JSON_ARRAY.
endmethod.


METHOD new_bigint.
  CREATE OBJECT er_json TYPE Zcl_jmp_json_data.
  IF iv_data IS SUPPLIED.
    er_json->create_bigint( iv_data ).
  ENDIF.
ENDMETHOD.


method NEW_BOOLEAN.
  CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
  IF iv_data IS SUPPLIED.
    er_json->CREATE_BOOLEAN( iv_data ).
  ENDIF.
endmethod.


method NEW_DOUBLE.
  CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
  IF iv_data IS SUPPLIED.
    er_json->create_double( iv_data ).
  ENDIF.
endmethod.


method NEW_FILE.

  DATA: lv_filesize LIKE iv_filesize.
  DATA: lv_filename LIKE iv_filename.
  DATA: lv_mimetype LIKE iv_mimetype.
  DATA: lv_prefix TYPE string.
  DATA: lv_ext TYPE string.
  DATA: lv_data TYPE string.
  DATA: lr_json TYPE REF TO ZCL_JMP_json.

  CREATE OBJECT er_json TYPE ZCL_JMP_json_data.

  IF iv_file IS SUPPLIED.
    lv_filename = iv_filename.

    lv_mimetype = iv_mimetype.
    IF lv_mimetype EQ space.
      IF lv_filename NE space.
        SPLIT lv_filename AT '.' INTO lv_prefix lv_ext.
        IF lv_ext NE space.
          CONCATENATE 'application'
                      lv_ext
                      INTO lv_mimetype
                      SEPARATED BY '/'.
        ENDIF.
      ENDIF.
      IF lv_mimetype EQ space.
        lv_mimetype = 'application/pdf'.
      ENDIF.
    ENDIF.

    lv_filesize = iv_filesize.
    IF lv_filesize < 1.
      lv_filesize = XSTRLEN( iv_file ).
    ENDIF.

    lr_json = ZCL_JMP_json=>get_instance( ).
    lv_data = lr_json->base64_encode( iv_file ).
    IF lv_data IS NOT INITIAL.
      CONCATENATE 'data:'
                  lv_mimetype
                  ';base64,'
                  lv_data
                  INTO lv_data.
      IF lv_filename NE space.
        CONCATENATE lv_data
                    ';'
                    lv_filename
                    INTO lv_data.
      ENDIF.
      er_json->create_string( lv_data ).
    ENDIF.
  ENDIF.


endmethod.


method NEW_INTEGER.
  CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
  IF iv_data IS SUPPLIED.
    er_json->CREATE_INTEGER( iv_data ).
  ENDIF.
endmethod.


method NEW_NULL.
  CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
  er_json->create_null( ).
endmethod.


method NEW_OBJECT.
  CREATE OBJECT er_json type ZCL_JMP_JSON_OBJECT.
endmethod.


method NEW_STRING.
  CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
  IF iv_data IS SUPPLIED.
    er_json->create_string( iv_data ).
  ENDIF.
endmethod.


method NEW_TIMESTAMP.

  DATA: lv_date TYPE sydatum.
  DATA: lv_time TYPE syuzeit.
  DATA: lv_string TYPE string.
  DATA: lv_tz TYPE tznzone VALUE 'UTC'.

  CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
  IF iv_data IS SUPPLIED.

    CONVERT TIME STAMP iv_data
          TIME ZONE lv_tz
          INTO DATE lv_date TIME lv_time
          "[DAYLIGHT SAVING TIME dst]
          .

    CONCATENATE lv_date(4)
                '-'
                lv_date+4(2)
                '-'
                lv_date+6(2)
                ' '
                lv_time(2)
                ':'
                lv_time+2(2)
                ':'
                lv_time+2(2)
                INTO lv_string
                RESPECTING BLANKS.

    er_json->create_string( lv_string ).
  ENDIF.
endmethod.


METHOD parse.

* ------- local data
  DATA: lr_parser TYPE REF TO Zcl_jmp_json_parser.
  DATA: lv_json TYPE string.

* ------- create a parser instance
  CREATE OBJECT lr_parser.


* ------- check input
  lv_json = iv_json.
  CHECK lv_json IS NOT INITIAL.


* ------- parse as string
  CALL METHOD lr_parser->parse
    EXPORTING
      iv_json      = lv_json
      ir_extension = extension
    IMPORTING
      er_json      = er_json
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

ENDMETHOD.


method SET_DECIMALS.
  IF iv_decimals GE 0.
    decimals = iv_decimals.
  ENDIF.
endmethod.


method SET_EXTENSION.
  extension = ir_extension.
endmethod.
ENDCLASS.
