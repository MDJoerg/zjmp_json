class ZCL_JMP_JSON_MAPPER definition
  public
  create public .

public section.
*"* public components of class ZCL_JMP_JSON_MAPPER
*"* do not include other source files here!!!

  data JSON type ref to ZCL_JMP_JSON .

  methods GET_DDIC_INFO
    importing
      !IV_DATA type ANY
    returning
      value(ES_DDIC) type DFIES .
  type-pools ABAP .
  methods GET_STRUCTURE_INFO
    importing
      !IV_STRUC type DATA
      !IV_LANGUAGE type SYLANGU default SY-LANGU
    exporting
      !ET_COMPONENT type ABAP_COMPONENT_TAB
      !ET_DDIC type DDFIELDS
      !EV_REL_NAME type STRING
    exceptions
      FAILED .
  methods IS_BOOLEAN_FROM_DDIC
    importing
      !IS_DDIC type DFIES
    returning
      value(EV_BOOLEAN) type ABAP_BOOL .
  methods MAP_FROM_JSON
    importing
      !IS_DDIC type DFIES optional
      value(IR_JSON) type ref to ZIF_JMP_JSON
    changing
      !CV_DATA type ANY
      !CV_MESSAGE type STRING
    exceptions
      FAILED .
  methods MAP_TO_JSON
    importing
      !IV_DATA type ANY
      !IS_DDIC type DFIES optional
    returning
      value(ER_JSON) type ref to ZIF_JMP_JSON .
  methods CONSTRUCTOR
    importing
      !IV_LANGUAGE type SYLANGU default SY-LANGU .
protected section.
*"* protected components of class ZCL_JMP_JSON_MAPPER
*"* do not include other source files here!!!

  data LANGUAGE type SYLANGU .
private section.
*"* private components of class ZCL_JMP_JSON_MAPPER
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_JMP_JSON_MAPPER IMPLEMENTATION.


method CONSTRUCTOR.
  me->language = iv_language.
endmethod.


method GET_DDIC_INFO.

*C  Zeichenfolge  (Character)
*N  Zeichenfolge nur mit Ziffern
*D  Datum (Date: JJJJMMTT)
*T  Zeitpunkt (Time: HHMMSS)
*X  Bytefolge (heXadecimal)
*I  Ganze Zahl (4-Byte Integer mit Vorzeichen)
*b  1-Byte Integer, ganze Zahl <= 254
*s  2-Byte Integer, nur für Längenfeld vor LCHR oder LRAW
*P  Gepackte Zahl (Packed)
*F  Gleitpunktzahl (Float) mit 8 Byte Genauigkeit
*g  Zeichenfolge mit variabler Länge (ABAP-Typ STRING)
*y  Bytefolge mit variabler Länge (ABAP-Typ XSTRING)
*u  Strukturierter Typ, flach
*v  Strukturierter Typ, tief
*h  Tabellentyp
*V  Zeichenfolge (alter Dictionary-Typ VARC)
*r  Referenz auf Klasse/Interface
*l  Referenz auf Datenobjekt


* -------- local data
  DATA: lv_inttype TYPE inttype.
  DATA: lr_typedescr TYPE REF TO cl_abap_typedescr.
  DATA: lr_abapdescr TYPE REF TO cl_abap_elemdescr.


* -------- get ddic
  " check type
  lr_typedescr = cl_abap_typedescr=>describe_by_data( iv_data ).
  CHECK lr_typedescr IS NOT INITIAL.

  lv_inttype = lr_typedescr->type_kind.
  CASE lv_inttype.
*     tables and structures
    WHEN 'h' OR 'v' OR 'u'.
      CLEAR es_ddic.
      es_ddic-inttype = lv_inttype.
      es_ddic-fieldname = lr_typedescr->get_relative_name( )..
*     not handled
    WHEN 'r' OR 'l'.
*     all others are abap types
    WHEN OTHERS.
      lr_abapdescr ?= lr_typedescr.
      CALL METHOD lr_abapdescr->get_ddic_field
        EXPORTING
          p_langu      = me->language
        RECEIVING
          p_flddescr   = es_ddic
        EXCEPTIONS
          not_found    = 1
          no_ddic_type = 2
          OTHERS       = 3.
  ENDCASE.

endmethod.


METHOD get_structure_info.

* ----- local data
  DATA: lr_rtti_struc TYPE REF TO cl_abap_structdescr.
  DATA: lr_elem  TYPE REF TO cl_abap_datadescr.
  DATA: lt_comp TYPE abap_component_tab.
  DATA: ls_comp LIKE LINE OF lt_comp.
  DATA: lv_type TYPE string.
  DATA: ls_ddic LIKE LINE OF et_ddic.


* ----- get access to
  lr_rtti_struc ?= cl_abap_structdescr=>describe_by_data( iv_struc ). " get the description of your <fs>
  IF lr_rtti_struc IS INITIAL.
    RAISE failed.
  ELSE.
    ev_rel_name  = lr_rtti_struc->get_relative_name( ).
  ENDIF.

* ----- fill export for a ddic structure
  CALL METHOD lr_rtti_struc->get_ddic_field_list
    EXPORTING
      p_langu                  = iv_language
      p_including_substructres = abap_false
    RECEIVING
      p_field_list             = et_ddic
    EXCEPTIONS
      not_found                = 1
      no_ddic_type             = 2
      OTHERS                   = 3.
  CHECK et_ddic IS INITIAL.

* ------ fill export for a generic structure
  lt_comp = lr_rtti_struc->get_components( ).
  LOOP AT lt_comp INTO ls_comp.
*   prepare
    lr_elem ?= ls_comp-type.
    lv_type = lr_elem->absolute_name.
    REPLACE '\TYPE=' IN lv_type WITH ''.
*   append ls_ddic
    CLEAR ls_ddic.
    ls_ddic-tabname     = ev_rel_name.
    ls_ddic-fieldname   = ls_comp-name.
    ls_ddic-position    = sy-tabix.
    ls_ddic-leng        = lr_elem->length.
    ls_ddic-decimals    = lr_elem->decimals.
    ls_ddic-domname     = lv_type.
    ls_ddic-inttype     = lr_elem->type_kind.
    APPEND ls_ddic TO et_ddic.
  ENDLOOP.

ENDMETHOD.


method IS_BOOLEAN_FROM_DDIC.
  ev_boolean = abap_false.
  CHECK is_ddic IS NOT INITIAL.

  IF    is_ddic-inttype = 'C'
    AND is_ddic-leng = 1
    AND ( is_ddic-domname = 'XFELD'
         OR is_ddic-domname = 'FLAG'
         OR is_ddic-rollname = 'BAPI_FLAG'
        ).
    ev_boolean = abap_true.
  ENDIF.

endmethod.


METHOD map_from_json.

* ------- local data
  DATA: lv_inttype    TYPE inttype.
  DATA: lv_string     TYPE string.
  DATA: lv_date       TYPE sydatum.
  DATA: lv_time       TYPE syuzeit.
  DATA: lv_checked_id TYPE string.
  DATA: lv_boolean    TYPE xfeld.
  DATA: lv_integer    TYPE i.
  DATA: lv_count      TYPE i.
  DATA: lv_len        TYPE i.
  DATA: lv_index      TYPE i.
  DATA: lv_double     TYPE Zjmp_json_double.
  DATA: ls_header     TYPE x030l.
  DATA: lt_ddic       TYPE ddfields.
  DATA: ls_ddic       LIKE LINE OF lt_ddic.
  DATA: lr_wa         TYPE REF TO data.
  DATA: lr_tab        TYPE REF TO data.
  DATA: lr_data       TYPE REF TO Zif_jmp_json.
  DATA: lr_object     TYPE REF TO Zcl_jmp_json_object.
  DATA: lr_array      TYPE REF TO Zcl_jmp_json_array.
  DATA: lr_value      TYPE REF TO Zcl_jmp_json_data.
  DATA: lr_typedescr  TYPE REF TO cl_abap_typedescr.

  FIELD-SYMBOLS: <data> TYPE data.
  FIELD-SYMBOLS: <tab>  TYPE ANY TABLE.
  FIELD-SYMBOLS: <wa>   TYPE data.

* ---------- get internal type and ddic infos
  CLEAR ls_ddic.
  IF is_ddic IS NOT INITIAL.
    ls_ddic    = is_ddic.
  ELSE.
    ls_ddic = me->get_ddic_info( cv_data ).
  ENDIF.

* ----------- check inttype is known
  lv_inttype = ls_ddic-inttype.
  CHECK lv_inttype IS NOT INITIAL.



* ----------- process dependend on type
  CASE lv_inttype.
      " string
    WHEN 'C' OR 'N' OR 'V' OR 'g'.
      IF ir_json->is_null( ) EQ abap_true.
        CLEAR cv_data.
      ELSEIF ls_ddic IS NOT INITIAL
        AND me->is_boolean_from_ddic( ls_ddic ) EQ abap_true.
        lr_value ?= ir_json.
        cv_data = lr_value->get_boolean( ).
      ELSE.
        lr_value ?= ir_json.
        cv_data = lr_value->get_string( ).
      ENDIF.
      " date
    WHEN 'D'.
      IF ir_json->is_null( ) EQ abap_true.
        CLEAR cv_data.
      ELSE.
        lv_string = ir_json->to_string( abap_false ).
        lv_len = strlen( lv_string ).
        " date is given in sap format
        IF lv_len EQ 8 AND lv_string CO '0123456789'.
          IF lv_string IS INITIAL
            OR lv_string CO ' .'
            OR lv_string EQ '00000000'
            OR lv_string EQ '00010101'.
            CLEAR cv_data.
          ELSE.
            cv_data = lv_string.
          ENDIF.
        ELSEIF lv_len = 10 AND lv_string CO '0123456789-'.
          CONCATENATE lv_string(4)
                      lv_string+5(2)
                      lv_string+8(2)
                      INTO lv_string.
          cv_data = lv_string.
        ELSE.
          " wrong format or not implemented
          cv_data = lv_string.
        ENDIF.
      ENDIF.
      " time
    WHEN 'T'.
      IF ir_json->is_null( ) EQ abap_true.
        CLEAR cv_data.
      ELSE.
        lv_string = ir_json->to_string( abap_false ).
        lv_len = strlen( lv_string ).
        IF lv_len EQ 6 AND lv_string CO '0123456789'.
          IF lv_string IS INITIAL
            OR lv_string EQ '000000'.
            CLEAR cv_data.
          ELSE.
            cv_data = lv_string.
          ENDIF.
        ELSEIF lv_len = 8 AND lv_string CO '0123456789:'.
          CONCATENATE lv_string(2)
                      lv_string+3(2)
                      lv_string+6(2)
                      INTO lv_string.
          cv_data = lv_string.
        ELSE.
          " wrong format or not implemented
          cv_data = lv_string.
        ENDIF.
      ENDIF.
      " integer
    WHEN 'I' OR 'b' OR 's'.
      lr_value ?= ir_json.
      cv_data = lr_value->get_integer( ).
      " double
    WHEN 'P' OR 'F'.
      lr_value ?= ir_json.
      cv_data = lr_value->get_double( ).
*   STRUCTURE
    WHEN 'v' OR 'u'.
      CALL METHOD me->get_structure_info
        EXPORTING
          iv_struc     = cv_data
          iv_language  = me->language
        IMPORTING
*         et_component = et_component
          et_ddic      = lt_ddic
*         ev_rel_name  = ev_rel_name
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
      IF sy-subrc <> 0 OR lt_ddic IS INITIAL.
        cv_message = 'wrong structure detected'.
        RAISE failed.
      ELSE.
        LOOP AT  lt_ddic INTO ls_ddic.
          UNASSIGN: <data>.
          ASSIGN COMPONENT ls_ddic-fieldname OF STRUCTURE cv_data TO <data>.
          IF <data> IS ASSIGNED.
            lr_object ?= ir_json.

            CLEAR lv_checked_id.
            lv_checked_id = lr_object->get_id_for_uppercase( ls_ddic-fieldname ).

            CLEAR lr_data.
            IF lv_checked_id IS NOT INITIAL.
              lr_data = lr_object->get( lv_checked_id ).
            ENDIF.

            IF lr_data IS NOT INITIAL.
              CALL METHOD me->map_from_json
                EXPORTING
                  is_ddic    = ls_ddic
                  ir_json    = lr_data
                CHANGING
                  cv_data    = <data>
                  cv_message = cv_message
                EXCEPTIONS
                  failed     = 1
                  OTHERS     = 2.
              IF sy-subrc <> 0.
                RAISE failed.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
*   Tables
    WHEN 'h'.
      FIELD-SYMBOLS: <any> TYPE any.

      IF ls_ddic-rollname IS NOT INITIAL.
        CREATE DATA lr_tab TYPE (ls_ddic-rollname).
        ASSIGN lr_tab->* TO <tab>.
      ELSE.
        ASSIGN ('cv_data[]') TO <any>.
        IF <any> IS ASSIGNED.
          ASSIGN ('cv_data[]') TO <tab>.
        ELSE.
          ASSIGN cv_data TO <tab>.
        ENDIF.
      ENDIF.

      IF <tab> IS ASSIGNED.

        CREATE DATA lr_wa LIKE LINE OF <tab>.
        ASSIGN lr_wa->* TO <wa>.

        lr_array ?= ir_json.
        IF lr_array IS NOT INITIAL
          AND lr_array->length( ) GT 0.
          lv_count = lr_array->length( ).
          DO lv_count TIMES.
            lv_index = sy-index - 1.
            lr_data = lr_array->get( lv_index ).
            IF lr_data IS NOT INITIAL.
              CLEAR <wa>.
              CALL METHOD me->map_from_json
                EXPORTING
*                 is_ddic    = is_ddic
                  ir_json    = lr_data
                CHANGING
                  cv_data    = <wa>
                  cv_message = cv_message
                EXCEPTIONS
                  failed     = 1
                  OTHERS     = 2.
              IF sy-subrc <> 0.
                RAISE failed.
              ELSE.
                INSERT <wa> INTO TABLE <tab>.
              ENDIF.
            ENDIF.
          ENDDO.
        ENDIF.
      ENDIF.

      cv_data = <tab>.
*   x value
    WHEN 'X' OR 'y'.
      IF ir_json->is_null( ) EQ abap_true.
        CLEAR cv_data.
      ELSE.
        lv_string = ir_json->to_string( abap_false ).
        cv_data = lv_string.
      ENDIF.
*   not supported type
    WHEN OTHERS.
      CONCATENATE 'unsupported type:' lv_inttype
        INTO cv_message SEPARATED BY ' '.
      RAISE failed.
  ENDCASE.


ENDMETHOD.


METHOD map_to_json.

* ------- local data
  DATA: lv_inttype TYPE inttype.
  DATA: lv_string TYPE string.
  DATA: lv_date TYPE sydatum.
  DATA: lv_time TYPE syuzeit.
  DATA: lv_integer TYPE i.
  DATA: lv_boolean TYPE xfeld.
  DATA: lv_double TYPE Zjmp_json_double.
  DATA: lt_ddic TYPE ddfields.
  DATA: ls_ddic LIKE LINE OF lt_ddic.
  FIELD-SYMBOLS: <wa> TYPE data.
  FIELD-SYMBOLS: <data> TYPE data.
  FIELD-SYMBOLS: <tab> TYPE table.
  DATA: lr_wa TYPE REF TO data.
  DATA: lr_data TYPE REF TO Zif_jmp_json.
  DATA: lr_object TYPE REF TO Zcl_jmp_json_object.
  DATA: lr_array  TYPE REF TO Zcl_jmp_json_array.


* ---------- get internal type and ddic infos
  CLEAR ls_ddic.
  IF is_ddic IS NOT INITIAL.
    ls_ddic    = is_ddic.
  ELSE.
    ls_ddic = me->get_ddic_info( iv_data ).
  ENDIF.

* ----------- check inttype is known
  lv_inttype = ls_ddic-inttype.
  CHECK lv_inttype IS NOT INITIAL.


* ----------- process data type
  CASE lv_inttype.
      " string
    WHEN 'C' OR 'N' OR 'V' OR 'g'.
      IF ls_ddic IS NOT INITIAL
        AND me->is_boolean_from_ddic( ls_ddic ) EQ abap_true.
        lv_boolean = iv_data.
        er_json = me->json->new_boolean( lv_boolean ).
      ELSE.
        lv_string = iv_data.
        IF lv_inttype = 'g' AND iv_data IS INITIAL.
          er_json = me->json->new_null(  ).
        ELSE.
          er_json = me->json->new_string( lv_string ).
        ENDIF.
      ENDIF.
      " date
    WHEN 'D'.
      lv_date = iv_data.
      IF lv_date IS INITIAL
        OR lv_date CO ' .'
        OR lv_date EQ '00000000'
        OR lv_date EQ '00010101'.
        er_json = me->json->new_null(  ).
      ELSE.
        CONCATENATE lv_date(4)
                    lv_date+4(2)
                    lv_date+6(2)
                    INTO lv_string
                    SEPARATED BY '-'.
        er_json = me->json->new_string( lv_string ).
      ENDIF.
      " time
    WHEN 'T'.
      lv_time = iv_data.
      IF lv_time IS INITIAL
        OR lv_time EQ '000000'
        OR lv_time CO ' :'.
        er_json = me->json->new_null(  ).
      ELSE.
        CONCATENATE lv_time(2)
                    lv_time+2(2)
                    lv_time+4(2)
                    INTO lv_string
                    SEPARATED BY ':'.
        er_json = me->json->new_string( lv_string ).
      ENDIF.
      " integer
    WHEN 'I' OR 'b' OR 's'.
      lv_integer = iv_data.
      er_json = me->json->new_integer( lv_integer ).
      " double
    WHEN 'P' OR 'F'.
      lv_double = iv_data.
      er_json = me->json->new_double( lv_double ).
      "when 'T'.
      " not supported
      "when 'X' or 'y'.
*   STRUCTURE
    WHEN 'v' OR 'u'.
      CALL METHOD me->get_structure_info
        EXPORTING
          iv_struc     = iv_data
          iv_language  = sy-langu
        IMPORTING
*         et_component = et_component
          et_ddic      = lt_ddic
*         ev_rel_name  = ev_rel_name
        EXCEPTIONS
          failed       = 1
          OTHERS       = 2.
      IF sy-subrc <> 0 OR lt_ddic IS INITIAL.
        EXIT.
      ELSE.
        lr_object = me->json->new_object( ).
        LOOP AT  lt_ddic INTO ls_ddic.
          UNASSIGN: <data>.
          ASSIGN COMPONENT ls_ddic-fieldname OF STRUCTURE iv_data TO <data>.
          IF <data> IS ASSIGNED.
            CLEAR lr_data.
            lr_data = me->map_to_json( iv_data = <data> ).
            IF lr_data IS NOT INITIAL.
              lv_string = ls_ddic-fieldname.
              lr_object->put( iv_id = lv_string ir_data = lr_data ).
            ENDIF.
          ENDIF.
        ENDLOOP.
        er_json ?= lr_object.
      ENDIF.
*   Tables
    WHEN 'h'.
      lr_array = me->json->new_array( ).

      IF iv_data IS NOT INITIAL.
        ASSIGN ('iv_data[]') TO <tab>.
        IF <tab> IS ASSIGNED.

          CREATE DATA lr_wa LIKE LINE OF <tab>.
          ASSIGN lr_wa->* TO <wa>.

          LOOP AT <tab> INTO <wa>.
            CLEAR lr_data.
            lr_data = me->map_to_json( iv_data = <wa> ).
            IF lr_data IS NOT INITIAL.
              lr_array->put( lr_data ).
            ENDIF.
          ENDLOOP.

        ENDIF.
      ENDIF.

      er_json ?= lr_array.
*   xstring
    WHEN 'X' OR 'y'.
      IF iv_data IS  INITIAL.
        er_json = me->json->new_null(  ).
      ELSE.
        lv_string = iv_data.
        er_json = me->json->new_string( lv_string ).
      ENDIF.
*   not supported type
    WHEN OTHERS.
  ENDCASE.


ENDMETHOD.
ENDCLASS.
