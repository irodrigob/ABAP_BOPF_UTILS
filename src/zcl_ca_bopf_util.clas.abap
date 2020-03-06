CLASS zcl_ca_bopf_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF cs_message,
                 error       TYPE sy-msgty VALUE 'E',
                 success     TYPE sy-msgty VALUE 'S',
                 information TYPE sy-msgty VALUE 'I',
                 dump        TYPE sy-msgty VALUE 'X',
                 termination TYPE sy-msgty VALUE 'A',
               END OF cs_message .

    "! <p class="shorttext synchronized">Constructor</p>
    METHODS constructor
      IMPORTING iv_bo_key TYPE /bobf/obm_bo_key
      RAISING   /bobf/cx_frw.
    "! <p class="shorttext synchronized">Modify and save data</p>
    METHODS modify_save_data
      IMPORTING
        !it_mod      TYPE /bobf/t_frw_modification
        !iv_langu    TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !eo_message  TYPE REF TO /bobf/if_frw_message
        !et_return   TYPE bapiret2_t
        !ev_rejected TYPE sap_bool .
    "! <p class="shorttext synchronized">Modify data</p>
    METHODS modify_data
      IMPORTING
        !it_mod     TYPE /bobf/t_frw_modification
        !iv_langu   TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !eo_message TYPE REF TO /bobf/if_frw_message
        !et_return  TYPE bapiret2_t .
    "! <p class="shorttext synchronized">Save data</p>
    METHODS save_data
      IMPORTING
        !iv_langu    TYPE sylangu DEFAULT sy-langu
      EXPORTING
        !eo_message  TYPE REF TO /bobf/if_frw_message
        !et_return   TYPE bapiret2_t
        !ev_rejected TYPE sap_bool .
    "! <p class="shorttext synchronized">Convertr BOPF message to bapiret2_t</p>
    METHODS conv_message_bopf_2_return
      IMPORTING
        !io_message TYPE REF TO /bobf/if_frw_message
        !iv_langu   TYPE sylangu DEFAULT sy-langu
      CHANGING
        !ct_return  TYPE bapiret2_t .
  PROTECTED SECTION.
    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_conf_mngr TYPE REF TO /bobf/if_frw_configuration.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ca_bopf_util IMPLEMENTATION.


  METHOD constructor.
    " Se instancian las clases del BOPF
    TRY.
        " Inicialización del gestor transaccional actualizaciones, bloqueos, etc..
        mo_txn_mngr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

        " Creación del el gestor de servicios del BOPF. Permite realizar las llamadas al BOPF para ejecutar validaciones, acciones, añadir, etc..
        " Es la clase más importante ya que toda la gestión CRUD se realiza en esta clase
        mo_svc_mngr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key ).

        " Creación de la configuración del BOPF, permite obtener los metadas del BOPF
        mo_conf_mngr = /bobf/cl_frw_factory=>get_configuration( iv_bo_key ).

      CATCH /bobf/cx_frw.
        "TODO: Error handling...
    ENDTRY.
  ENDMETHOD.


  METHOD conv_message_bopf_2_return.
    io_message->get_messages( IMPORTING et_message = DATA(lt_messages) ).

    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_messages>).

      DATA(lo_msg) = CAST /bobf/cm_frw_symsg( <ls_messages>-message ).

      INSERT zcl_ca_utilities=>fill_return( iv_type       = cs_message-error
                                            iv_number     = <ls_messages>-message->if_t100_message~t100key-msgno
                                            iv_id         = <ls_messages>-message->if_t100_message~t100key-msgid
                                            iv_message_v1 = lo_msg->mv_attr1
                                            iv_message_v2 = lo_msg->mv_attr2
                                            iv_message_v3 = lo_msg->mv_attr3
                                            iv_message_v4 = lo_msg->mv_attr4
                                            iv_langu      = iv_langu ) INTO TABLE ct_return.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_data.
    CLEAR: eo_message, et_return.

    mo_svc_mngr->modify(
      EXPORTING
        it_modification = it_mod
      IMPORTING
        eo_change       = DATA(lo_change)
        eo_message      = eo_message ).

    " Si hay mensajes y el parametro de ET_RETURN esta informado los convierto
    IF eo_message IS BOUND AND et_return IS SUPPLIED.
      conv_message_bopf_2_return(
        EXPORTING
          io_message = eo_message
          iv_langu   = iv_langu
        CHANGING
          ct_return  = et_return ).

    ENDIF.


  ENDMETHOD.


  METHOD modify_save_data.

    CLEAR: eo_message, et_return.
    ev_rejected = abap_false.

    modify_data(
      EXPORTING
        it_mod     = it_mod
        iv_langu   = iv_langu
    IMPORTING
      eo_message = eo_message
      et_return  = et_return ).

    READ TABLE et_return TRANSPORTING NO FIELDS WITH KEY type = cs_message-error.
    IF sy-subrc NE 0. " Sin errores se graba
      save_data(
        EXPORTING
          iv_langu    = iv_langu
        IMPORTING
          eo_message  = DATA(lo_message)
          et_return   =  DATA(lt_return)
          ev_rejected =  ev_rejected ).

      " Si hay error en la grabación devuelvo los mensajes de grabación
      IF ev_rejected = abap_true.
        eo_message = lo_message.
        et_return = lt_return.
      ENDIF.

    ELSE.
      ev_rejected = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD save_data.
    CLEAR: et_return, eo_message.

    ev_rejected = abap_false.
    mo_txn_mngr->save(
        IMPORTING
          ev_rejected            = ev_rejected
          eo_message             = eo_message ).

    " Si hay mensajes y el parametro de ET_RETURN esta informado los convierto
    IF eo_message IS BOUND AND et_return IS SUPPLIED.
      conv_message_bopf_2_return(
        EXPORTING
          io_message = eo_message
          iv_langu   = iv_langu
        CHANGING
          ct_return  = et_return ).

    ENDIF.
  ENDMETHOD.
ENDCLASS.
