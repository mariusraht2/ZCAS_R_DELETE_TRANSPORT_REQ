CLASS zz_cl_delete_transport_request DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING VALUE(ro_instance) TYPE REF TO zz_cl_delete_transport_request.

    METHODS:
      execute
        IMPORTING iv_user     TYPE as4user
                  it_r_trkorr TYPE rseloption,
      on_value_request_so_tr
        IMPORTING iv_user          TYPE as4user
        RETURNING VALUE(rv_result) TYPE shvalue_d.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: mo_instance   TYPE REF TO zz_cl_delete_transport_request,
                mv_user       TYPE as4user,
                mt_r_trkorr   TYPE rseloption,
                mt_r_strkorr  TYPE rseloption,
                mt_log        TYPE log_message_t,
                mo_salv_table TYPE REF TO cl_salv_table.

    METHODS:
      delete_tr,
      read_tr,
      unlock_tr,
      show_log,
      on_function_click_log_alv FOR EVENT added_function OF cl_salv_events_table
        IMPORTING e_salv_function .
ENDCLASS.



CLASS ZZ_CL_DELETE_TRANSPORT_REQUEST IMPLEMENTATION.


  METHOD delete_tr.

    LOOP AT mt_r_strkorr ASSIGNING FIELD-SYMBOL(<ls_r_strkorr>).

      DATA(lv_trkorr) = CONV trkorr( <ls_r_strkorr>-low ).

      CALL FUNCTION 'TR_DELETE_COMM'
        EXPORTING
          wi_dialog = abap_false
          wi_trkorr = lv_trkorr
        EXCEPTIONS
          OTHERS    = 1.

      CASE sy-subrc.
        WHEN 0.
          APPEND VALUE #( type    = 'S'
                          message = |Transport request { lv_trkorr } and its sub requests were successfully deleted.|  ) TO mt_log.

        WHEN OTHERS.
          APPEND VALUE #( type    = 'W'
                          message = |Transport request { lv_trkorr } and its sub requests couldn't be deleted.|  ) TO mt_log.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD execute.

    CLEAR: mt_log.

    mv_user     = iv_user.
    mt_r_trkorr = it_r_trkorr.

    read_tr( ).
    unlock_tr( ).
    delete_tr( ).

    show_log( ).

  ENDMETHOD.


  METHOD get_instance.

    IF mo_instance IS NOT BOUND.

      mo_instance = NEW zz_cl_delete_transport_request( ).

    ENDIF.

    ro_instance = mo_instance.

  ENDMETHOD.


  METHOD on_function_click_log_alv.

    mo_salv_table->close_screen( ).

  ENDMETHOD.


  METHOD on_value_request_so_tr.

    DATA: lt_results TYPE STANDARD TABLE OF ddshretval WITH DEFAULT KEY.

    SELECT *
      FROM e070
      INTO TABLE @DATA(lt_help_values)
      WHERE trstatus EQ 'D'
        AND as4user  EQ @iv_user.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        ddic_structure = 'E070'
        retfield       = 'TRKORR'
        dynpprog       = sy-repid
        dynpnr         = sy-dynnr
        window_title   = 'Transport Request'
        value_org      = 'S'
      TABLES
        value_tab      = lt_help_values
        return_tab     = lt_results
      EXCEPTIONS
        OTHERS         = 1.

    IF sy-subrc <> 0.
      MESSAGE 'Build of Search Help-Pop Up failed.' TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.

    IF lines( lt_results ) > 0.
      rv_result = lt_results[ 1 ]-fieldval.
    ENDIF.

  ENDMETHOD.


  METHOD read_tr.

    DATA: lt_e070 TYPE TABLE OF e070 WITH DEFAULT KEY.

    CHECK mt_r_trkorr IS NOT INITIAL.

    SELECT *
      FROM e070
      INTO TABLE @lt_e070
      WHERE trstatus EQ 'D'
        AND as4user  EQ @mv_user
        AND (    trkorr  IN @mt_r_trkorr
              OR strkorr IN @mt_r_trkorr ).

    mt_r_strkorr = VALUE rseloption( FOR <line1> IN lt_e070 WHERE ( strkorr CN ' _0' ) ( sign   = 'I'
                                                                                         option = 'EQ'
                                                                                         low    = <line1>-strkorr ) ).

    IF mt_r_strkorr IS NOT INITIAL.

      SELECT *
        FROM e070
        APPENDING TABLE @lt_e070
        WHERE trstatus EQ 'D'
          AND as4user  EQ @mv_user
          AND (    trkorr  IN @mt_r_strkorr
                OR strkorr IN @mt_r_strkorr ).

    ENDIF.

    IF lt_e070 IS NOT INITIAL.

      SORT lt_e070 BY trkorr.
      DELETE ADJACENT DUPLICATES FROM lt_e070.

      mt_r_trkorr = VALUE #( FOR <line2> IN lt_e070 ( sign   = 'I'
                                                      option = 'EQ'
                                                      low    = <line2>-trkorr ) ).

    ELSE.

      CLEAR: mt_r_trkorr, mt_r_strkorr.
      MESSAGE 'No valid open transport request could be found.' TYPE 'W' DISPLAY LIKE 'I'.

    ENDIF.

  ENDMETHOD.


  METHOD show_log.

    TYPES: BEGIN OF t_log,
             severity TYPE icon_d,
             message  TYPE string,
           END OF t_log.

    DATA: lt_log     TYPE TABLE OF t_log WITH DEFAULT KEY,
          lo_events  TYPE REF TO cl_salv_events_table,
          lo_column  TYPE REF TO cl_salv_column_list,
          lo_columns TYPE REF TO cl_salv_columns_table,
          lt_columns TYPE salv_t_column_ref.

    FIELD-SYMBOLS: <ls_column> TYPE salv_s_column_ref.

    CHECK mt_log IS NOT INITIAL.

    lt_log = CORRESPONDING #( mt_log MAPPING severity = type
                                             message  = message ).
    LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<ls_log>).

      CASE <ls_log>-severity.
        WHEN 'S'.
          <ls_log>-severity = icon_okay.

        WHEN 'W'.
          <ls_log>-severity = icon_warning.

        WHEN 'E'.
          <ls_log>-severity = icon_failure.

      ENDCASE.

    ENDLOOP.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = mo_salv_table
                                CHANGING  t_table      = lt_log ).

        mo_salv_table->set_screen_status( pfstatus = '110'
                                          report = 'SAPMSVIM' ).

        mo_salv_table->set_screen_popup( start_column = 1
                                         end_column   = 80
                                         start_line   = 4
                                         end_line     = 15 ).

        mo_salv_table->get_display_settings( )->set_list_header( 'Log' ).
        mo_salv_table->get_display_settings( )->set_striped_pattern( abap_true ).
        mo_salv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>none ).

        lo_events = mo_salv_table->get_event( ).
        SET HANDLER: on_function_click_log_alv FOR lo_events.

        lo_columns = mo_salv_table->get_columns( ).
        lt_columns = lo_columns->get( ).

        lo_column ?= lo_columns->get_column( 'SEVERITY' ).
        lo_column->set_icon( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( 'Severity' ).
        lo_column->set_output_length( 5 ).

        lo_column ?= lo_columns->get_column( 'MESSAGE' ).
        lo_column->set_long_text( 'Message' ).
        lo_column->set_output_length( 72 ).

        mo_salv_table->display( ).

      CATCH cx_root.
        MESSAGE 'Build of Log Table failed.' TYPE 'E' DISPLAY LIKE 'I'.

    ENDTRY.

  ENDMETHOD.


  METHOD unlock_tr.

    LOOP AT mt_r_trkorr ASSIGNING FIELD-SYMBOL(<ls_r_trkorr>).

      DATA(lv_trkorr) = CONV trkorr( <ls_r_trkorr>-low ).

      CALL FUNCTION 'TRINT_UNLOCK_COMM'
        EXPORTING
          wi_trkorr = lv_trkorr
        EXCEPTIONS
          OTHERS    = 1.

      CASE sy-subrc.
        WHEN 0.
          APPEND VALUE #( type    = 'S'
                          message = |Transport request { lv_trkorr } was successfully unlocked.|  ) TO mt_log.

        WHEN OTHERS.
          APPEND VALUE #( type    = 'W'
                          message = |Transport request { lv_trkorr } couldn't be unlocked.|  ) TO mt_log.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
