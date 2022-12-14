class ZCL_CA_BOPF_UTIL definition
  public
  final
  create public .

public section.

  constants:
    BEGIN OF cs_message,
                 error       TYPE sy-msgty VALUE 'E',
                 success     TYPE sy-msgty VALUE 'S',
                 information TYPE sy-msgty VALUE 'I',
                 dump        TYPE sy-msgty VALUE 'X',
                 termination TYPE sy-msgty VALUE 'A',
               END OF cs_message .

    "! <p class="shorttext synchronized">Constructor</p>
  methods CONSTRUCTOR
    importing
      !IV_BO_KEY type /BOBF/OBM_BO_KEY
    raising
      /BOBF/CX_FRW .
    "! <p class="shorttext synchronized">Modify and save data</p>
  methods MODIFY_SAVE_DATA
    importing
      !IT_MOD type /BOBF/T_FRW_MODIFICATION
      !IV_LANGU type SYLANGU default SY-LANGU
    exporting
      !EO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
      !ET_RETURN type BAPIRET2_T
      !EV_REJECTED type SAP_BOOL .
    "! <p class="shorttext synchronized">Modify data</p>
  methods MODIFY_DATA
    importing
      !IT_MOD type /BOBF/T_FRW_MODIFICATION
      !IV_LANGU type SYLANGU default SY-LANGU
    exporting
      !EO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
      !ET_RETURN type BAPIRET2_T .
    "! <p class="shorttext synchronized">Save data</p>
  methods SAVE_DATA
    importing
      !IV_LANGU type SYLANGU default SY-LANGU
    exporting
      !EO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
      !ET_RETURN type BAPIRET2_T
      !EV_REJECTED type SAP_BOOL .
    "! <p class="shorttext synchronized">Convert BOPF message to bapiret2_t</p>
  methods CONV_MESSAGE_BOPF_2_RETURN
    importing
      !IO_MESSAGE type ref to /BOBF/IF_FRW_MESSAGE
      !IV_LANGU type SYLANGU default SY-LANGU
    changing
      !CT_RETURN type BAPIRET2_T .
    "! <p class="shorttext synchronized">Rollback data</p>
  methods ROLLBACK
    importing
      !IV_CLEANUP_MODE type /BOBF/CONF_CLEANUP_MODE optional
      !IV_LANGU type SYLANGU default SY-LANGU
    exporting
      !ET_RETURN type BAPIRET2_T .
  methods ODATA_FILTERS_TO_BOPF
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
      !IO_DP_FACADE type ref to /IWBEP/CL_MGW_DP_FACADE
      !IV_ENTITY_NAME type STRING
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION
    returning
      value(RT_PARAMS) type /BOBF/T_FRW_QUERY_SELPARAM .
  PROTECTED SECTION.
    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_conf_mngr TYPE REF TO /bobf/if_frw_configuration.
private section.
ENDCLASS.



CLASS ZCL_CA_BOPF_UTIL IMPLEMENTATION.


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

      INSERT zcl_ca_utilities=>fill_return( iv_type       = COND #( WHEN lo_msg->severity IS NOT INITIAL THEN lo_msg->severity ELSE cs_message-error )
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


  METHOD odata_filters_to_bopf.

    DATA: lo_model        TYPE REF TO /iwbep/if_mgw_odata_re_model,
          lt_entity_props TYPE /iwbep/if_mgw_odata_re_prop=>ty_t_mgw_odata_properties,
          ls_filter       LIKE LINE OF it_filter_select_options.

    FIELD-SYMBOLS: <fs_range> TYPE table.

    "Get Model
    lo_model = io_dp_facade->/iwbep/if_mgw_dp_int_facade~get_model( ).

    "Get Entity Properties.
    lt_entity_props = lo_model->get_entity_type( |{ iv_entity_name }| )->get_properties( ).

    "Entities y conversor
    DATA(lt_entities) = lo_model->get_entity_types( ).
    DATA(lo_selopt_conv) = io_tech_request_context->get_filter( ).

    "Mapeamos los filtros si existen la entidad en el modelo oData importado
    READ TABLE lt_entities ASSIGNING FIELD-SYMBOL(<fs_enti>)
    WITH KEY name = iv_entity_name.
    IF sy-subrc EQ 0.

      LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<fs_filter>).

        READ TABLE lt_entity_props ASSIGNING FIELD-SYMBOL(<fs_entity>)
        WITH KEY name = <fs_filter>-property.
        IF sy-subrc EQ 0.

          CLEAR ls_filter.
          ls_filter = CORRESPONDING #( <fs_filter> ).
          ls_filter-property = <fs_entity>-technical_name.

          READ TABLE <fs_enti>-properties ASSIGNING FIELD-SYMBOL(<fs_prop>)
          WITH KEY external_name = <fs_entity>-name.
          IF sy-subrc EQ 0.

            zcl_ca_dynamic_tables=>create_it_from_fcat(
            EXPORTING
              i_fields = VALUE #( ( inttype = cl_abap_datadescr=>typekind_char
                                    intlen  = 2
                                    fieldname = 'OPTION' )
                                  ( inttype = cl_abap_datadescr=>typekind_char
                                    intlen  = 1
                                    fieldname = 'SIGN' )
                                  ( inttype = <fs_prop>-internal_type
                                    intlen  = <fs_prop>-length
                                    fieldname = 'LOW' )
                                  ( inttype = <fs_prop>-internal_type
                                    intlen  = <fs_prop>-length
                                    fieldname = 'HIGH' ) )   " Catálogo campos p.ListViewerControl
            IMPORTING
              e_table  = DATA(eo_et_data)
              e_wa     = DATA(eo_es_data)
          ).

          ENDIF.

          ASSIGN eo_et_data->* TO <fs_range>.

          lo_selopt_conv->convert_select_option(
            EXPORTING
              is_select_option = ls_filter
            IMPORTING
              et_select_option = <fs_range>                 " Ranges table
          ).

          "Añadimos todos los filtros que se han enviado
          LOOP AT <fs_range> ASSIGNING FIELD-SYMBOL(<fs_selopt>).

            APPEND INITIAL LINE TO rt_params ASSIGNING FIELD-SYMBOL(<fs_param>).
            <fs_param> = CORRESPONDING #( <fs_selopt> ).
            <fs_param>-attribute_name = <fs_entity>-technical_name.

          ENDLOOP.

        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD rollback.
    CLEAR et_return.
    DATA(lo_message) = mo_txn_mngr->cleanup( iv_cleanup_mode = iv_cleanup_mode ).

    IF lo_message IS BOUND AND et_return IS SUPPLIED.
      conv_message_bopf_2_return(
          EXPORTING
            io_message = lo_message
            iv_langu   = iv_langu
          CHANGING
            ct_return  = et_return ).
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
