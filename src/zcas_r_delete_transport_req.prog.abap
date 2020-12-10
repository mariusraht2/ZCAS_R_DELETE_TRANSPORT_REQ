*&---------------------------------------------------------------------*
*& Report zz_r_delete_transport_request
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcas_r_delete_transport_req.

TABLES: e070.

PARAMETERS: p_user TYPE tr_as4user DEFAULT sy-uname.
SELECT-OPTIONS: so_tr FOR e070-trkorr.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_tr-low.
  so_tr-low = zcas_cl_delete_transport_req=>get_instance( )->on_value_request_so_tr( p_user ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_tr-high.
  so_tr-high = zcas_cl_delete_transport_req=>get_instance( )->on_value_request_so_tr( p_user ).

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'ONLI'.
      DATA(lt_r_trkorr) = VALUE rseloption( FOR <line1> IN so_tr ( sign   = <line1>-sign
                                                                   option = <line1>-option
                                                                   low    = <line1>-low
                                                                   high   = <line1>-high ) ).

      zcas_cl_delete_transport_req=>get_instance( )->execute( iv_user     = p_user
                                                                it_r_trkorr = lt_r_trkorr ).

  ENDCASE.

*call function ''
