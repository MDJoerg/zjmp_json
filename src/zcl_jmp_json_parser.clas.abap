class ZCL_JMP_JSON_PARSER definition
  public
  create public .

public section.
*"* public components of class ZCL_JMP_JSON_PARSER
*"* do not include other source files here!!!

  methods PARSE
    importing
      !IV_JSON type STRING
      !IR_EXTENSION type ref to ZIF_JMP_JSON_EXTENSION optional
    exporting
      !ER_JSON type ref to ZIF_JMP_JSON
    exceptions
      FAILED .
protected section.
*"* protected components of class ZCL_JMP_JSON_PARSER
*"* do not include other source files here!!!

  data EXTENSION type ref to ZIF_JMP_JSON_EXTENSION .
private section.
*"* private components of class ZCL_JMP_JSON_PARSER
*"* do not include other source files here!!!

  methods GET_ANY
    importing
      !IV_JSON type STRING
    exporting
      !EV_FOUND type FLAG
      !ES_DATA type ZUT_DATA
    changing
      !CV_POS type I .
  methods GET_ARRAY
    importing
      !IV_JSON type STRING
      !IR_JSON type ref to ZCL_JMP_JSON_ARRAY
    exporting
      !EV_FOUND type FLAG
      !ET_ARRAY type ZUT_ARRAY_TAB
    changing
      !CV_POS type I .
  methods GET_BOOLEAN
    importing
      !IV_JSON type STRING
    exporting
      !EV_FOUND type FLAG
      !EV_BOOLEAN type FLAG
    changing
      !CV_POS type I .
  methods GET_ELEMENT
    importing
      !IV_JSON type STRING
    exporting
      !ES_ELEMENT type ZUT_HASH_ELEMENT
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_HASH
    importing
      !IV_JSON type STRING
      !IR_JSON type ref to ZCL_JMP_JSON_OBJECT
    exporting
      !ET_HASH type ZUT_HASH_TAB
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_KEY
    importing
      !IV_JSON type STRING
    exporting
      !EV_KEY type STRING
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_NAME
    importing
      !IV_JSON type STRING
    exporting
      !EV_NAME type STRING
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_NULL
    importing
      !IV_JSON type STRING
    exporting
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods GET_NUMBER
    importing
      !IV_JSON type STRING
    exporting
      !EV_FOUND type FLAG
      !EV_NUMBER type ref to DATA
      !EV_INTEGER type FLAG
    changing
      !CV_POS type I .
  methods GET_STRING
    importing
      !IV_JSON type STRING
      !IV_DELIMITER type CHAR01 optional
    exporting
      !EV_FOUND type FLAG
      !EV_STRING type STRING
    changing
      !CV_POS type I .
  methods GET_SYMBOL
    importing
      !IV_JSON type STRING
      !IV_SYMBOL type CSEQUENCE
    exporting
      !EV_FOUND type FLAG
    changing
      !CV_POS type I .
  methods MAP_CHAR
    importing
      !IV_CHAR type CSEQUENCE
    returning
      value(EV_MAPPED_CHAR) type CHAR01 .
  methods SKIP_WHITESPACE
    importing
      !IV_JSON type STRING
    changing
      !CV_POS type I .
ENDCLASS.



CLASS ZCL_JMP_JSON_PARSER IMPLEMENTATION.


METHOD get_any.
  DATA: ls_data      TYPE zut_data,
        lv_pos       TYPE i.

  DATA: lr_json TYPE REF TO Zcl_jmp_json.

  FIELD-SYMBOLS: <lv_boolean> TYPE flag,
                 <lv_string>  TYPE string,
                 <lt_hash>    TYPE zut_hash_tab,
                 <lt_array>   TYPE zut_array_tab.

  ev_found = space.

  lv_pos = cv_pos.

  skip_whitespace( EXPORTING iv_json = iv_json
                   CHANGING  cv_pos  = lv_pos ).
  CHECK lv_pos < strlen( iv_json ).

  lr_json = Zcl_jmp_json=>get_instance( ).

  CASE iv_json+lv_pos(1).
    WHEN '"' OR `'`.
      CREATE DATA ls_data-data TYPE string.
      ASSIGN ls_data-data->* TO <lv_string>.
      ls_data-type = 'S'.

      CALL METHOD get_string
        EXPORTING
          iv_json   = iv_json
        IMPORTING
          ev_string = <lv_string>
          ev_found  = ev_found
        CHANGING
          cv_pos    = lv_pos.


      ls_data-json ?= lr_json->new_string( <lv_string> ).

    WHEN '{'.
      DATA: lr_object TYPE REF TO Zcl_jmp_json_object.
      lr_object = lr_json->new_object( ).
      ls_data-json ?= lr_object.

      CREATE DATA ls_data-data TYPE zut_hash_tab.
      ASSIGN ls_data-data->* TO <lt_hash>.
      ls_data-type = 'h'.

      CALL METHOD get_hash
        EXPORTING
          iv_json  = iv_json
          ir_json  = lr_object
        IMPORTING
          et_hash  = <lt_hash>
          ev_found = ev_found
        CHANGING
          cv_pos   = lv_pos.
    WHEN '['.
      DATA: lr_array TYPE REF TO Zcl_jmp_json_array.
      lr_array = lr_json->new_array( ).
      ls_data-json ?= lr_array.

      CREATE DATA ls_data-data TYPE zut_array_tab.
      ASSIGN ls_data-data->* TO <lt_array>.
      ls_data-type = 'a'.

      CALL METHOD get_array
        EXPORTING
          iv_json  = iv_json
          ir_json  = lr_array
        IMPORTING
          et_array = <lt_array>
          ev_found = ev_found
        CHANGING
          cv_pos   = lv_pos.


    WHEN 't' OR 'f'.
      CREATE DATA ls_data-data TYPE flag.
      ASSIGN ls_data-data->* TO <lv_boolean>.
      ls_data-type = 'B'.
      CALL METHOD get_boolean
        EXPORTING
          iv_json    = iv_json
        IMPORTING
          ev_boolean = <lv_boolean>
          ev_found   = ev_found
        CHANGING
          cv_pos     = lv_pos.

      ls_data-json ?= lr_json->new_boolean( <lv_boolean> ).
    WHEN 'n'.
      CLEAR ls_data-data.
      ls_data-type = 'N'.
      CALL METHOD get_null
        EXPORTING
          iv_json  = iv_json
        IMPORTING
          ev_found = ev_found
        CHANGING
          cv_pos   = lv_pos.

      ls_data-json ?= lr_json->new_null( ).
    WHEN OTHERS.
      DATA: lv_integer          TYPE abap_bool.
      FIELD-SYMBOLS: <integer>  TYPE i.
      FIELD-SYMBOLS: <double>   TYPE f.
      FIELD-SYMBOLS: <bigint>   TYPE Zjmp_json_bigint.

      IF iv_json+lv_pos(1) CA '0123456789-'.
        ls_data-type = 'N'.


        CALL METHOD get_number
          EXPORTING
            iv_json    = iv_json
          IMPORTING
            ev_number  = ls_data-data
            ev_found   = ev_found
            ev_integer = lv_integer
          CHANGING
            cv_pos     = lv_pos.

        IF ev_found EQ abap_true.
          IF lv_integer EQ 'B'.  " workaround BIGINT
            ASSIGN ls_data-data->* TO <bigint>.
            ls_data-json ?= lr_json->new_bigint( <bigint> ).
          ELSEIF lv_integer EQ abap_true.
            ASSIGN ls_data-data->* TO <integer>.
            ls_data-json ?= lr_json->new_integer( <integer> ).
          ELSE.
            ASSIGN ls_data-data->* TO <double>.
            ls_data-json ?= lr_json->new_double( <double> ).
          ENDIF.
        ENDIF.
      ENDIF.
  ENDCASE.

  IF ev_found = 'X'.
    cv_pos  = lv_pos.
    es_data = ls_data.
  ENDIF.

ENDMETHOD.


method GET_ARRAY.
  DATA: ls_element  TYPE zut_data,
      lv_pos      TYPE i,
      lv_found    TYPE flag,
      lv_key      TYPE string.

  CLEAR ev_found.
  lv_pos = cv_pos.


  get_symbol( EXPORTING iv_json = iv_json
                        iv_symbol = '['
              IMPORTING ev_found = lv_found
              CHANGING  cv_pos  = lv_pos ).
  CHECK lv_found = 'X'.

  WHILE lv_found EQ 'X'.

    CALL METHOD get_any
      EXPORTING
        iv_json  = iv_json
      IMPORTING
        es_data  = ls_element
        ev_found = lv_found
      CHANGING
        cv_pos   = lv_pos.

    IF lv_found = 'X'.

      INSERT ls_element INTO TABLE et_array.

      data: lr_json type REF TO ZIF_JMP_json.
      if ir_json is NOT INITIAL
        and ls_element-json is NOT INITIAL.
        lr_json ?= ls_element-json.
        ir_json->put( lr_json ).
      endif.

      CALL METHOD get_symbol
        EXPORTING
          iv_json   = iv_json
          iv_symbol = ','
        IMPORTING
          ev_found  = lv_found
        CHANGING
          cv_pos    = lv_pos.

    ENDIF.

  ENDWHILE.


  get_symbol( EXPORTING iv_json = iv_json
                        iv_symbol = ']'
              IMPORTING ev_found = lv_found
              CHANGING  cv_pos  = lv_pos ).
  CHECK lv_found = 'X'.

  ev_found = 'X'.
  cv_pos = lv_pos.
endmethod.


method GET_BOOLEAN.
  DATA: lv_symbol TYPE string,
      lv_boolean TYPE flag.


  CHECK cv_pos < STRLEN( iv_json ).

  CASE iv_json+cv_pos(1).
    WHEN 't'.
      lv_symbol = 'true'.
      lv_boolean = 'X'.
    WHEN 'f'.
      lv_symbol = 'false'.
      lv_boolean = space.
  ENDCASE.

  CALL METHOD get_symbol
    EXPORTING
      iv_json   = iv_json
      iv_symbol = lv_symbol
    IMPORTING
      ev_found  = ev_found
    CHANGING
      cv_pos    = cv_pos.

  IF ev_found = 'X'.
    ev_boolean = lv_boolean.
  ENDIF.
endmethod.


method GET_ELEMENT.
  DATA: lv_pos     TYPE i,
      lv_found   TYPE flag,
      ls_element TYPE zut_hash_element.

  CLEAR: ev_found, es_element.

  lv_pos = cv_pos.

* Schlüssel
  CALL METHOD get_key
    EXPORTING
      iv_json  = iv_json
    IMPORTING
      ev_key   = ls_element-key
      ev_found = lv_found
    CHANGING
      cv_pos   = lv_pos.
  CHECK lv_found EQ 'X'.

* Doppelpunkt
  get_symbol( EXPORTING iv_json = iv_json
                        iv_symbol = ':'
              IMPORTING ev_found = lv_found
              CHANGING  cv_pos  = lv_pos ).
  CHECK lv_found = 'X'.

* Wert
  CALL METHOD get_any
    EXPORTING
      iv_json  = iv_json
    IMPORTING
      es_data  = ls_element-value
      ev_found = lv_found
    CHANGING
      cv_pos   = lv_pos.

  IF lv_found = 'X'.
    ev_found   = 'X'.
    cv_pos     = lv_pos.
    es_element = ls_element.
  ENDIF.
endmethod.


method GET_HASH.

  data: ls_element  type zut_hash_element,
        lv_pos      type i,
        lv_found    type flag,
        lv_key      type string.

  clear ev_found.
  lv_pos = cv_pos.


  get_symbol( exporting iv_json = iv_json
                        iv_symbol = '{'
              importing ev_found = lv_found
              changing  cv_pos  = lv_pos ).
  check lv_found = 'X'.

  while lv_found eq 'X'.

    call method get_element
      exporting
        iv_json    = iv_json
      importing
        es_element = ls_element
        ev_found   = lv_found
      changing
        cv_pos     = lv_pos.

    if lv_found = 'X'.

      insert ls_element into table et_hash.

      data: lr_json type REF TO ZIF_JMP_json.
      if ir_json is NOT INITIAL
        and ls_element-json is NOT INITIAL.
        lr_json ?= ls_element-json.
        ir_json->put( iv_id = ls_element-key ir_data = lr_json ).
      endif.


      call method get_symbol
        exporting
          iv_json   = iv_json
          iv_symbol = ','
        importing
          ev_found  = lv_found
        changing
          cv_pos    = lv_pos.

    endif.

  endwhile.


  get_symbol( exporting iv_json = iv_json
                        iv_symbol = '}'
              importing ev_found = lv_found
              changing  cv_pos  = lv_pos ).
  check lv_found = 'X'.

  ev_found = 'X'.
  cv_pos = lv_pos.
endmethod.


method GET_KEY.

  data: lv_pos          type i,
        lv_found        type flag.

  ev_found = space.
  check cv_pos < strlen( iv_json ).

  lv_pos = cv_pos.

* Ist der Key als String notiert?
  call method get_string
    exporting
      iv_json   = iv_json
    importing
      ev_string = ev_key
      ev_found  = lv_found
    changing
      cv_pos    = lv_pos.

  if lv_found = space.
* Zweiter Versuch: Symbolischer Name
    call method get_name
      exporting
        iv_json  = iv_json
      importing
        ev_name  = ev_key
        ev_found = lv_found
      changing
        cv_pos   = lv_pos.
  endif.

  if lv_found = 'X'.
    ev_found = lv_found.
    cv_pos   = lv_pos.
  endif.
endmethod.


method GET_NAME.
  DATA: lv_name         TYPE string,
      lv_length       TYPE i.

  ev_found = ev_name = space.
  CHECK cv_pos < STRLEN( iv_json ).

  FIND REGEX '^\s*([a-z_]\w*)' IN SECTION OFFSET cv_pos OF iv_json
                           SUBMATCHES lv_name
                           MATCH LENGTH lv_length.
  IF sy-subrc EQ 0.
    ev_found = 'X'.
    cv_pos   = cv_pos + lv_length.
    ev_name  = lv_name.
  ENDIF.
endmethod.


method GET_NULL.
  DATA: lv_symbol TYPE string.

  CHECK cv_pos < STRLEN( iv_json ).

  lv_symbol = 'null'.

  CALL METHOD get_symbol
    EXPORTING
      iv_json   = iv_json
      iv_symbol = lv_symbol
    IMPORTING
      ev_found  = ev_found
    CHANGING
      cv_pos    = cv_pos.

endmethod.


METHOD get_number.

  DATA: lv_pos    TYPE i,
        lv_exp    TYPE string,
        lv_length TYPE i.

  FIELD-SYMBOLS: <lv_number> TYPE numeric.

  CLEAR ev_number.
  ev_found = space.
  lv_pos = cv_pos.

  FIND REGEX '^\s*([\d.-]+(e-?\d+)?)' IN iv_json+lv_pos SUBMATCHES lv_exp MATCH LENGTH lv_length.
  IF sy-subrc EQ 0.
    ADD lv_length TO lv_pos.
* Ganze Zahl?
    IF lv_exp CO '-0123456789'.
      CREATE DATA ev_number TYPE i.
      ev_integer = abap_true.
    ELSE.
      FIND REGEX '^\d*\.\d+|\d+\.\d*$' IN lv_exp.
      IF sy-subrc EQ 0.
        CREATE DATA ev_number TYPE f.
        ev_integer = abap_false.
      ENDIF.
    ENDIF.

    IF ev_number IS BOUND.
* Hier überlassen wir die Feinheiten des Parsings dem ABAP-Befehl MOVE:
      TRY.
          ASSIGN ev_number->* TO <lv_number>.
          <lv_number> = lv_exp.
        CATCH cx_sy_conversion_overflow.
          CREATE DATA ev_number TYPE Zjmp_json_bigint.
          ASSIGN ev_number->* TO <lv_number>.
          <lv_number> = lv_exp.
          ev_integer = 'B'.
      ENDTRY.


      ev_found = 'X'.
    ENDIF.

  ENDIF.


  IF ev_found = 'X'.
    cv_pos = lv_pos.
  ENDIF.
ENDMETHOD.


method GET_STRING.
  DATA: lv_pos             TYPE i,
          lv_delimiter(1)    TYPE c,
          lv_char(1)         TYPE c,
          lv_mapped_char(1)  TYPE c.

  DATA: lv_data_pos TYPE i.
  DATA: lv_data_out TYPE string.
  DATA: lv_data_beg TYPE i.
  DATA: lv_new_pos TYPE i.

  ev_found = space.

  lv_pos = cv_pos.
  CALL METHOD skip_whitespace
    EXPORTING
      iv_json = iv_json
    CHANGING
      cv_pos  = lv_pos.

  CHECK lv_pos < STRLEN( iv_json ).

  IF iv_delimiter IS NOT INITIAL.
    lv_delimiter = iv_delimiter.
    CHECK iv_json+lv_pos(1) EQ lv_delimiter.
  ELSE.
    lv_delimiter = iv_json+lv_pos(1).
    CHECK lv_delimiter CA `'"`.
  ENDIF.


  DO.

    ADD 1 TO lv_pos.

    IF STRLEN( iv_json ) <= lv_pos.
      EXIT.
    ENDIF.

* Escaped sequences finden und auflösen
    FIND REGEX `^\\(['"/bfnrt\\])` IN SECTION OFFSET lv_pos OF iv_json SUBMATCHES lv_char.
    IF sy-subrc EQ 0.
      IF lv_char CA `bfnrt`.
        lv_mapped_char = map_char( iv_char = lv_char ).
      ELSE.
        lv_mapped_char = lv_char.  " Else auch hier, wg. Performance
      ENDIF.
      CONCATENATE ev_string lv_mapped_char INTO ev_string.
      ADD 1 TO lv_pos.
      CONTINUE.
    ENDIF.

    IF iv_json+lv_pos(1) EQ lv_delimiter.
      ev_found = 'X'.
      EXIT.
    ENDIF.

    CONCATENATE ev_string iv_json+lv_pos(1) INTO ev_string.

*   special performant processing of data strings
    IF ev_string EQ 'data:'.
      lv_data_beg = lv_pos + 1.
      IF iv_json+lv_data_beg CS '"'.
        lv_data_pos = sy-fdpos.
        IF lv_data_pos GT 0.
          CONCATENATE ev_string
                      iv_json+lv_data_beg(lv_data_pos)
                      INTO ev_string.
          lv_pos = lv_pos + lv_data_pos + 1.
          REPLACE ALL  OCCURRENCES OF '\\' IN ev_string WITH ''.
          ev_found = 'X'.
          EXIT. " from do
        ENDIF.
      ENDIF.
    ENDIF.
  ENDDO.

*  check for unicode unescaping
  IF extension IS NOT INITIAL AND ev_string CS '\u'.
    me->extension->unescape_external_string( CHANGING cv_string = ev_string ).
  ENDIF.

  IF ev_found = 'X'.
    ADD 1 TO lv_pos.
    cv_pos = lv_pos.
  ENDIF.
endmethod.


method GET_SYMBOL.
  clear ev_found.

  skip_whitespace(
    exporting iv_json = iv_json
    changing  cv_pos  = cv_pos ).

  check cv_pos < strlen( iv_json ).

  if iv_json+cv_pos cs iv_symbol and sy-fdpos = 0.
    ev_found = 'X'.
    cv_pos = cv_pos + strlen( iv_symbol ).
  endif.
endmethod.


method MAP_CHAR.

  CASE iv_char.
    WHEN 'b'.
      ev_mapped_char = cl_abap_char_utilities=>backspace.
    WHEN 'f'.
      ev_mapped_char = cl_abap_char_utilities=>form_feed.
    WHEN 'n'.
      ev_mapped_char = cl_abap_char_utilities=>newline.
    WHEN 'r'.
      ev_mapped_char = cl_abap_char_utilities=>cr_lf(1).
    WHEN 't'.
      ev_mapped_char = cl_abap_char_utilities=>horizontal_tab.
    WHEN OTHERS.
      ev_mapped_char = iv_char.
  ENDCASE.

endmethod.


METHOD parse.

  DATA: lv_pos TYPE i,
        lv_found TYPE flag.
  DATA: ls_data TYPE zut_data.

  CLEAR ls_data.
  CHECK iv_json IS NOT INITIAL.

  me->extension = ir_extension.

* Einen JSON-Ausdruck auswerten
  get_any( EXPORTING iv_json = iv_json
           IMPORTING es_data = ls_data
                     ev_found = lv_found
           CHANGING  cv_pos  = lv_pos ).

  IF lv_found EQ space OR
* Hintendran darf nichts mehr kommen
    lv_pos < strlen( iv_json ).
    FIND REGEX '\S' IN SECTION OFFSET lv_pos OF iv_json.
    IF sy-subrc EQ 0.
      RAISE failed. "exception type zcx_parse_error.
    ENDIF.
  ENDIF.

  er_json = ls_data-json.

ENDMETHOD.


method SKIP_WHITESPACE.
  data: lv_pos type i.

  find regex '(\S|\Z)' in section offset cv_pos of iv_json match offset lv_pos.
  if sy-subrc eq 0.
    cv_pos = lv_pos.
  endif.
endmethod.
ENDCLASS.
