*&---------------------------------------------------------------------*
*& Report zz_r_delete_transport_request
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcas_r_delete_transport.

INCLUDE zcas_r_delete_transport_top.
INCLUDE zcas_r_delete_transport_sel.
INCLUDE zcas_r_delete_transport_cls.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_tr-low.
  so_tr-low = lcl_application=>get( )->on_value_request_so_tr( p_user ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_tr-high.
  so_tr-high = lcl_application=>get( )->on_value_request_so_tr( p_user ).

AT SELECTION-SCREEN.
  lcl_application=>get( )->execute( iv_user     = p_user
                                    it_r_trkorr = CONV #( so_tr[] ) ).
