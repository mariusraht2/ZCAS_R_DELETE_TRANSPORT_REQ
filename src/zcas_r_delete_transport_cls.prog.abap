*&---------------------------------------------------------------------*
*& Include zcas_r_delete_tr_cls
*&---------------------------------------------------------------------*
CLASS lcl_application DEFINITION
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-METHODS get
      RETURNING
        VALUE(ro_instance) TYPE REF TO lcl_application.

    METHODS:
      execute
        IMPORTING
          iv_user     TYPE as4user
          it_r_trkorr TYPE rseloption,
      on_value_request_so_tr
        IMPORTING
          iv_user          TYPE as4user
        RETURNING
          VALUE(rv_result) TYPE shvalue_d.

  PROTECTED SECTION.
    TYPES: t_e070 TYPE TABLE OF e070 WITH DEFAULT KEY.

    CLASS-DATA: mo_instance   TYPE REF TO lcl_application,
                mv_user       TYPE as4user,
                mt_trkorr     TYPE TABLE OF e070 WITH DEFAULT KEY,
                mt_log        TYPE log_message_t,
                mv_msg        TYPE string,
                mo_salv_table TYPE REF TO cl_salv_table.

    METHODS:
      delete_tr,
      read_tr
        IMPORTING
          it_r_trkorr TYPE rseloption,
      unlock_tr,
      show_log,
      reopen_tr,
      on_function_click_log_alv
        FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
          e_salv_function .

ENDCLASS.


CLASS lcl_application IMPLEMENTATION.

  METHOD delete_tr.

    LOOP AT mt_trkorr ASSIGNING FIELD-SYMBOL(<ls_strkorr>) WHERE strkorr CO ' _0'.

      CALL FUNCTION 'TR_DELETE_COMM'
        EXPORTING
          wi_dialog = abap_false
          wi_trkorr = <ls_strkorr>-trkorr
        EXCEPTIONS
          OTHERS    = 1.

      CASE sy-subrc.
        WHEN 0.
          mv_msg = TEXT-001.
          REPLACE '&1' IN mv_msg WITH <ls_strkorr>-trkorr.
          APPEND VALUE #( type    = 'S'
                          message = mv_msg ) TO mt_log.

        WHEN OTHERS.
          mv_msg = TEXT-002.
          REPLACE '&1' IN mv_msg WITH <ls_strkorr>-trkorr.
          APPEND VALUE #( type    = 'W'
                          message = mv_msg  ) TO mt_log.
          CONTINUE.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD execute.

    CASE sy-ucomm.
      WHEN 'ONLI'.
        CLEAR: mt_log.
        mv_user = iv_user.

        read_tr( it_r_trkorr ).
        reopen_tr( ).
        unlock_tr( ).
        delete_tr( ).
        show_log( ).

    ENDCASE.

  ENDMETHOD.


  METHOD get.

    IF mo_instance IS NOT BOUND.
      mo_instance = NEW lcl_application( ).
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
        window_title   = TEXT-003
        value_org      = 'S'
      TABLES
        value_tab      = lt_help_values
        return_tab     = lt_results
      EXCEPTIONS
        OTHERS         = 1.

    IF sy-subrc <> 0.
      MESSAGE TEXT-004 TYPE 'E' DISPLAY LIKE 'I'.
    ENDIF.

    IF lines( lt_results ) > 0.
      rv_result = lt_results[ 1 ]-fieldval.
    ENDIF.

  ENDMETHOD.


  METHOD read_tr.

    CHECK mv_user CN ' _0'
      AND it_r_trkorr IS NOT INITIAL.

    " Scenarios:
    " > Delete parent requests (mode == 'D') and its child requests
    " > Delete single child requests (mode of parent request == 'D')

    " Get all selected requests
    SELECT *
      FROM e070
      INTO TABLE @DATA(lt_sel_requests)
      WHERE as4user EQ @mv_user
        AND trkorr  IN @it_r_trkorr.

    IF lt_sel_requests IS INITIAL.
      MESSAGE TEXT-005 TYPE 'E' DISPLAY LIKE 'W'.
    ENDIF.

    " Get all valid open parent requests
    SELECT *
      FROM e070
      INTO TABLE @DATA(lt_all_parent_requests)
      WHERE as4user  EQ @mv_user
        AND strkorr  EQ @space
        AND trstatus EQ 'D'.

    IF lt_all_parent_requests IS INITIAL.
      MESSAGE TEXT-005 TYPE 'E' DISPLAY LIKE 'W'.
    ENDIF.

    DATA(lt_r_all_parent_requests) = VALUE rseloption( FOR <line1> IN lt_all_parent_requests
                                                   ( sign   = 'I'
                                                     option = 'EQ'
                                                     low    = <line1>-trkorr ) ).

    " Get all selected parent requests which are valid
    DATA(lt_sel_parent_requests) = VALUE t_e070( FOR <line3> IN lt_sel_requests
                                                 WHERE (     strkorr CO ' _0'
                                                         AND trkorr IN lt_r_all_parent_requests )
                                                 ( <line3> ) ).

    DATA(lt_r_sel_parent_requests) = VALUE rseloption( FOR <line2> IN lt_sel_parent_requests
                                                       ( sign   = 'I'
                                                         option = 'EQ'
                                                         low    = <line2>-trkorr ) ).

    " Get all selected child requests which are valid
    DATA(lt_sel_child_requests) = VALUE t_e070( FOR <line3> IN lt_sel_requests
                                            WHERE (     strkorr CN ' _0'
                                                    AND strkorr IN lt_r_all_parent_requests )
                                            ( <line3> ) ).

    DATA(lt_r_sel_child_requests) = VALUE rseloption( FOR <line2> IN lt_sel_child_requests
                                                       ( sign   = 'I'
                                                         option = 'EQ'
                                                         low    = <line2>-trkorr ) ).

    " Get child requests by selected parent requests
    SELECT *
      FROM e070
      APPENDING TABLE @lt_sel_child_requests
      WHERE as4user EQ @mv_user
        AND strkorr IN @lt_r_sel_parent_requests
        AND trkorr  NOT IN @lt_r_sel_child_requests.

    APPEND LINES OF: lt_sel_parent_requests TO mt_trkorr,
                     lt_sel_child_requests  TO mt_trkorr.

    FREE: lt_sel_requests, lt_sel_child_requests, lt_all_parent_requests,
          lt_r_all_parent_requests, lt_sel_parent_requests, lt_r_sel_parent_requests,
          lt_r_sel_child_requests.

    IF mt_trkorr IS INITIAL.
      MESSAGE TEXT-005 TYPE 'E' DISPLAY LIKE 'W'.
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

        mo_salv_table->get_display_settings( )->set_list_header( TEXT-006 ).
        mo_salv_table->get_display_settings( )->set_striped_pattern( abap_true ).
        mo_salv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>none ).

        lo_events = mo_salv_table->get_event( ).
        SET HANDLER: on_function_click_log_alv FOR lo_events.

        lo_columns = mo_salv_table->get_columns( ).
        lt_columns = lo_columns->get( ).

        lo_column ?= lo_columns->get_column( 'SEVERITY' ).
        lo_column->set_icon( if_salv_c_bool_sap=>true ).
        lo_column->set_long_text( TEXT-007 ).
        lo_column->set_output_length( 5 ).

        lo_column ?= lo_columns->get_column( 'MESSAGE' ).
        lo_column->set_long_text( TEXT-008 ).
        lo_column->set_output_length( 72 ).

        mo_salv_table->display( ).

      CATCH cx_root.
        MESSAGE TEXT-009 TYPE 'E' DISPLAY LIKE 'I'.

    ENDTRY.

  ENDMETHOD.


  METHOD unlock_tr.

    LOOP AT mt_trkorr ASSIGNING FIELD-SYMBOL(<ls_trkorr>).

      CALL FUNCTION 'TRINT_UNLOCK_COMM'
        EXPORTING
          wi_trkorr = <ls_trkorr>-trkorr
        EXCEPTIONS
          OTHERS    = 1.

      CASE sy-subrc.
        WHEN 0.
          mv_msg = TEXT-010.
          REPLACE '&1' IN mv_msg WITH <ls_trkorr>-trkorr.
          APPEND VALUE #( type    = 'S'
                          message = mv_msg  ) TO mt_log.

        WHEN OTHERS.
          mv_msg = TEXT-011.
          REPLACE '&1' IN mv_msg WITH <ls_trkorr>-trkorr.
          APPEND VALUE #( type    = 'W'
                          message = mv_msg  ) TO mt_log.
          CONTINUE.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD reopen_tr.

    DATA: lv_jobname  TYPE tbtco-jobname VALUE 'DELETE_TR',
          lv_jobcount TYPE tbtco-jobcount.

    LOOP AT mt_trkorr ASSIGNING FIELD-SYMBOL(<ls_strkorr>) WHERE strkorr  CN ' _0'
                                                             AND trstatus EQ 'R'.

      <ls_strkorr>-trstatus = 'D'.
      <ls_strkorr>-as4date = sy-datum.
      <ls_strkorr>-as4time = sy-uzeit.
      <ls_strkorr>-as4user = sy-uname.
      UPDATE e070 FROM <ls_strkorr>.

      CASE sy-subrc.
        WHEN 0.
          mv_msg = TEXT-012.
          REPLACE '&1' IN mv_msg WITH <ls_strkorr>-trkorr.
          APPEND VALUE #( type    = 'S'
                          message = mv_msg  ) TO mt_log.

        WHEN OTHERS.
          mv_msg = TEXT-013.
          REPLACE '&1' IN mv_msg WITH <ls_strkorr>-trkorr.
          APPEND VALUE #( type    = 'E'
                          message = mv_msg  ) TO mt_log.
          CONTINUE.

      ENDCASE.

    ENDLOOP.

    COMMIT WORK AND WAIT.

  ENDMETHOD.

ENDCLASS.
