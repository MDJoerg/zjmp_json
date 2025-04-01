CLASS zcl_jmp_json DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_JMP_JSON
*"* do not include other source files here!!!

    INTERFACES zif_jmp_json .

    CLASS-DATA json_string TYPE zjmp_json_data_type VALUE 'S'. "#EC NOTEXT .
    CLASS-DATA json_boolean TYPE zjmp_json_data_type VALUE 'B'. "#EC NOTEXT .
    CLASS-DATA json_integer TYPE zjmp_json_data_type VALUE 'I'. "#EC NOTEXT .
    CLASS-DATA json_double TYPE zjmp_json_data_type VALUE 'D'. "#EC NOTEXT .
    CLASS-DATA json_object TYPE zjmp_json_data_type VALUE 'O'. "#EC NOTEXT .
    CLASS-DATA json_array TYPE zjmp_json_data_type VALUE 'A'. "#EC NOTEXT .
    CLASS-DATA json_null TYPE zjmp_json_data_type VALUE 'N'. "#EC NOTEXT .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(er_instance) TYPE REF TO zcl_jmp_json .

    CLASS-METHODS set_decimals
      IMPORTING
        !iv_decimals TYPE i DEFAULT 8 .

    METHODS get_datetime_from_unix_integer
      IMPORTING
        !iv_seconds_since_1970 TYPE i
      EXPORTING
        !ev_date               TYPE datum
        !ev_time               TYPE uzeit .
    CLASS-METHODS set_extension
      IMPORTING
        !ir_extension TYPE REF TO zif_jmp_json_extension .
    METHODS clone
      IMPORTING
        !ir_json       TYPE REF TO zif_jmp_json
      RETURNING
        VALUE(er_json) TYPE REF TO zcl_jmp_json .
    CLASS-METHODS parse
      IMPORTING
        !iv_json       TYPE any
      RETURNING
        VALUE(er_json) TYPE REF TO zif_jmp_json .
    CLASS-METHODS get_extension
      RETURNING
        VALUE(er_extension) TYPE REF TO zif_jmp_json_extension .
    METHODS new_timestamp
      IMPORTING
        !iv_data       TYPE timestampl OPTIONAL
      RETURNING
        VALUE(er_json) TYPE REF TO zcl_jmp_json_data .
    METHODS new_string
      IMPORTING
        !iv_data       TYPE string OPTIONAL
      RETURNING
        VALUE(er_json) TYPE REF TO zcl_jmp_json_data .
    METHODS new_object
      RETURNING
        VALUE(er_json) TYPE REF TO zcl_jmp_json_object .
    METHODS new_bigint
      IMPORTING
        !iv_data       TYPE data OPTIONAL
      RETURNING
        VALUE(er_json) TYPE REF TO zcl_jmp_json_data .
    METHODS new_integer
      IMPORTING
        !iv_data       TYPE i OPTIONAL
      RETURNING
        VALUE(er_json) TYPE REF TO zcl_jmp_json_data .
    METHODS new_file
      IMPORTING
        !iv_file       TYPE xstring OPTIONAL
        !iv_filename   TYPE string OPTIONAL
        !iv_filesize   TYPE i OPTIONAL
        !iv_mimetype   TYPE string OPTIONAL
      RETURNING
        VALUE(er_json) TYPE REF TO zcl_jmp_json_data .
    METHODS new_double
      IMPORTING
        !iv_data       TYPE zjmp_json_double OPTIONAL
      RETURNING
        VALUE(er_json) TYPE REF TO zcl_jmp_json_data .
    METHODS new_null
      RETURNING
        VALUE(er_json) TYPE REF TO zcl_jmp_json_data .
    "type-pools ABAP .
    METHODS new_boolean
      IMPORTING
        !iv_data       TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(er_json) TYPE REF TO zcl_jmp_json_data .
    METHODS new_array
      RETURNING
        VALUE(er_json) TYPE REF TO zcl_jmp_json_array .
    METHODS get_timestamp
      IMPORTING
        !ir_data            TYPE REF TO zif_jmp_json OPTIONAL
        !iv_data            TYPE string OPTIONAL
      RETURNING
        VALUE(ev_timestamp) TYPE timestampl .
    METHODS get_file
      IMPORTING
        !ir_data     TYPE REF TO zif_jmp_json
      EXPORTING
        !ev_file     TYPE xstring
        !ev_filename TYPE string
        !ev_mimetype TYPE string
        !ev_filesize TYPE i
        !ev_text     TYPE string .
    METHODS escape_string
      IMPORTING
        !iv_unescaped     TYPE string
      RETURNING
        VALUE(ev_escaped) TYPE string .
    METHODS base64_encode
      IMPORTING
        !iv_binary         TYPE xstring
      RETURNING
        VALUE(ev_b64coded) TYPE string .
    METHODS base64_decode
      IMPORTING
        VALUE(iv_b64coded) TYPE string
      RETURNING
        VALUE(ev_binary)   TYPE xstring .
  PROTECTED SECTION.
*"* protected components of class ZCL_JMP_JSON
*"* do not include other source files here!!!

    CLASS-DATA decimals TYPE i VALUE 8.                   "#EC NOTEXT .
    CLASS-DATA extension TYPE REF TO zif_jmp_json_extension .
  PRIVATE SECTION.
*"* private components of class ZCL_JMP_JSON
*"* do not include other source files here!!!

    CLASS-DATA instance TYPE REF TO zcl_jmp_json .
ENDCLASS.



CLASS zcl_jmp_json IMPLEMENTATION.


  METHOD Zif_jmp_json~get_data.
    FIELD-SYMBOLS: <data> TYPE data.
    ASSIGN ('me->data') TO <data>.
    IF <data> IS ASSIGNED.
      GET REFERENCE OF <data> INTO er_data.
    ENDIF.
  ENDMETHOD.


  METHOD base64_decode.
** copied from SSFC_BASE64_ENCODE
*  include_constants.
*  DATA: bincopy TYPE xstring.
*  DATA: binlen TYPE i.
*  DATA: strleng TYPE i.
*
*
*  CALL 'SSF_ABAP_SERVICE'                                 "#EC CI_CCALL
*    ID 'OPCODE'          FIELD   ssf_opcodes-base64decode
*    ID 'B64DATA'         FIELD   iv_b64coded
*    ID 'BINDATA'         FIELD   ev_binary.
*
*  IF sy-subrc > 0.
*  ENDIF.

  ENDMETHOD.


  METHOD base64_encode.
** copied from SSFC_BASE64_ENCODE
*  include_constants.
*  DATA: bincopy TYPE xstring.
*  DATA: binlen TYPE i.
*  DATA: strleng TYPE i.
*
**  if binleng = 0. "convert whole xstring
**    bincopy = bindata.
**  else.           "convert substring
**    strleng = xstrlen( bindata ).
**    if binleng < 0 or binleng > strleng.
**      message e310(1s) raising ssf_krn_invalid_parlen.
**    endif.
**    bincopy = bindata(binleng).
**  endif.
*
*  CALL 'SSF_ABAP_SERVICE'                                 "#EC CI_CCALL
*    ID 'OPCODE'          FIELD   ssf_opcodes-base64encode
*    ID 'BINDATA'         FIELD   iv_binary
*    ID 'B64DATA'         FIELD   ev_b64coded.
*
*  IF sy-subrc > 0.
**    case sy-subrc.
**      when ssfkrn_rc-krn_noop.
**        message e301(1s) raising ssf_krn_noop.
**      when ssfkrn_rc-krn_nomemory.
**        message e302(1s) raising ssf_krn_nomemory.
**      when ssfkrn_rc-krn_opinv.
**        message e303(1s) raising ssf_krn_opinv.
***   WHEN SSFKRN_RC-KRN_INPUT_DATA_ERROR.
***     MESSAGE E308(1S) RAISING SSF_KRN_INPUT_DATA_ERROR.
**      when ssfkrn_rc-krn_invalid_par.
**        message e309(1s) raising ssf_krn_invalid_par.
**      when others.
**        message e110(1s) raising ssf_krn_error.
**    endcase.
*  ENDIF.

  ENDMETHOD.


  METHOD clone.

* ------- local data
    DATA: lv_json TYPE string.

* ------- check
    CHECK ir_json IS NOT INITIAL.

* ------- get as string
    lv_json = ir_json->to_string( ).
    CHECK lv_json IS NOT INITIAL.

* ------- get as new object
    er_json ?= me->parse( lv_json ).

  ENDMETHOD.


  METHOD escape_string.
    "TODO
    "ev_escaped = cl_http_utility=>escape_javascript( iv_unescaped ).
    ev_escaped = iv_unescaped.
  ENDMETHOD.


  METHOD get_datetime_from_unix_integer.

    "TODO:
*  DATA: lv_time(8).
*
*  PERFORM p6_to_date_time_tz IN PROGRAM rstr0400
*    IF FOUND
*    USING iv_seconds_since_1970 lv_time ev_date.
*
*  CONCATENATE
*    lv_time(2)
*    lv_time+3(2)
*    lv_time+6(2)
*    INTO ev_time.

  ENDMETHOD.


  METHOD get_extension.
    er_extension ?= extension.
  ENDMETHOD.


  METHOD get_file.

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
              ev_filesize = xstrlen( ev_file ).
              ev_filename = lv_filename.
              ev_text     = lv_text.
              ev_mimetype = lv_mimetype.
              " bugfix
              REPLACE '\/' IN ev_mimetype WITH '/'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    IF ZCL_JMP_json=>instance IS INITIAL.
      CREATE OBJECT ZCL_JMP_json=>instance.
    ENDIF.
    er_instance ?= ZCL_JMP_json=>instance.
  ENDMETHOD.



  METHOD get_timestamp.

** ------ local data
*  DATA: lv_string(30).
*  DATA: lr_string TYPE REF TO ZCL_JMP_json_data.
*
** ------ check data type
*  IF ir_data IS NOT INITIAL.
*    IF ir_data->get_data_type( ) NE ZCL_JMP_json=>json_string.
*      EXIT.
*    ELSE.
*      lr_string ?= ir_data.
*      lv_string = lr_string->get_string( ).
*    ENDIF.
*  ELSEIF iv_data IS NOT INITIAL.
*    lv_string = iv_data.
*  ELSE.
*    EXIT. "no valid input
*  ENDIF.
*
** ----- check content
*  CHECK lv_string IS NOT INITIAL.
*
** ------
*  "          0123456789012345678901234567890
*  "check for YYYY-MM-DD HH:MM:SS (TIMEZONE)
*  DATA: lv_date TYPE sydatum.
*  DATA: lv_time TYPE syuzeit.
*  DATA: lv_timezone TYPE string.
*  DATA: lv_tz   TYPE timezone VALUE 'UTC'.
*  DATA: lv_pos TYPE i.
*
** ------ get date part
*  CHECK lv_string+4(1) EQ '-'
*    AND lv_string+7(1) EQ '-'
*    AND lv_string+10(1) EQ ' '.
*  lv_date(4)   = lv_string(4).
*  lv_date+4(2) = lv_string+5(2).
*  lv_date+6(2) = lv_string+8(2).
*
** ------ get time part
*  IF lv_string+13(1) = ':'
*    AND lv_string+16(1) = ':'.
*    lv_time(2)    = lv_string+11(2).
*    lv_time+2(2)  = lv_string+14(2).
*    lv_time+4(2)  = lv_string+17(2).
*  ELSE.
*    lv_time = '000000'.
*  ENDIF.
*
** ------ check for timezone
*  IF lv_string CS '('.
*    lv_pos = sy-fdpos + 1.
*    lv_timezone = lv_string+lv_pos.
*    IF lv_timezone CS ')'.
*      lv_pos = sy-fdpos.
*      lv_timezone = lv_timezone(lv_pos).
*
**     check timezone
*      SELECT SINGLE tzone FROM ttzz
*        INTO lv_tz
*        WHERE tzone EQ lv_timezone.
*      IF sy-subrc NE 0.
*        lv_tz = sy-zonlo.
*        IF extension IS NOT INITIAL.
*          CALL METHOD extension->map_external_timezone
*            EXPORTING
*              iv_timezone_ext = lv_timezone
*            CHANGING
*              cv_timezone_int = lv_tz.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
** ------ convert timezone
*  CONVERT DATE lv_date TIME lv_time
*  "[DAYLIGHT SAVING TIME dst]
*        INTO TIME STAMP ev_timestamp TIME ZONE lv_tz.

  ENDMETHOD.


  METHOD new_array.
    CREATE OBJECT er_json TYPE zcl_jmp_json_array.
  ENDMETHOD.


  METHOD new_bigint.
    CREATE OBJECT er_json TYPE Zcl_jmp_json_data.
    IF iv_data IS SUPPLIED.
      er_json->create_bigint( iv_data ).
    ENDIF.
  ENDMETHOD.


  METHOD new_boolean.
    CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
    IF iv_data IS SUPPLIED.
      er_json->create_boolean( iv_data ).
    ENDIF.
  ENDMETHOD.


  METHOD new_double.
    CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
    IF iv_data IS SUPPLIED.
      er_json->create_double( iv_data ).
    ENDIF.
  ENDMETHOD.


  METHOD new_file.

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
        lv_filesize = xstrlen( iv_file ).
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


  ENDMETHOD.


  METHOD new_integer.
    CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
    IF iv_data IS SUPPLIED.
      er_json->create_integer( iv_data ).
    ENDIF.
  ENDMETHOD.


  METHOD new_null.
    CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
    er_json->create_null( ).
  ENDMETHOD.


  METHOD new_object.
    CREATE OBJECT er_json TYPE zcl_jmp_json_object.
  ENDMETHOD.


  METHOD new_string.
    CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
    IF iv_data IS SUPPLIED.
      er_json->create_string( iv_data ).
    ENDIF.
  ENDMETHOD.


  METHOD new_timestamp.

    DATA: lv_date TYPE datum.
    DATA: lv_time TYPE uzeit.
    DATA: lv_string TYPE string.
    DATA: lv_tz TYPE tznzone VALUE 'UTC'.

    "CREATE OBJECT er_json TYPE ZCL_JMP_json_data.
    er_json = new_string( ).

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
  ENDMETHOD.


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


  METHOD set_decimals.
    IF iv_decimals GE 0.
      decimals = iv_decimals.
    ENDIF.
  ENDMETHOD.


  METHOD set_extension.
    extension = ir_extension.
  ENDMETHOD.
ENDCLASS.
