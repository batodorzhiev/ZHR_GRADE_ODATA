class ZCL_ZHR_KPI_APP_DPC_EXT definition
  public
  inheriting from ZCL_ZHR_KPI_APP_DPC
  create public .

public section.

  class-methods GET_ACTUAL_ROUTE_LINE
    importing
      !IT_ROUTE type ZHR_UI_APPROVER_T
      !IS_NEW_HEAD_REQUEST type ZHRT_UI_KPI_GRA
      !IS_OLD_HEAD_REQUEST type ZHRT_UI_KPI_GRA
      !IV_USER type SY-UNAME
      !IT_REQUEST type ZHR_UI_KPI_GRA_TT
      !IV_WAY type CHAR1
    returning
      value(ES_ROUTE) type ZHR_UI_APPROVER_S .
  class-methods READ_TEXT
    exporting
      !TEXT_ID type THEAD-TDNAME
    changing
      !TEXT type STRING .
  class-methods GET_HISTORY
    importing
      !IV_REQID type ZHRE_UI_REQID
      !IV_UNAME type UNAME
      !IV_DEL type FLAG optional
      !IV_YEAR type GJAHR default SY-DATUM+0(4)
    exporting
      !ET_ROUTE type ZHR_UI_APPROVER_T
      !ET_HISTORY_ROUTE type ZHR_UI_APPROVER_T
      !ET_REQUEST type ZHR_UI_KPI_GRA_TT
      !EV_TOP_UNAME type UNAME
      !EV_TOP_DELEG type UNAME
      !EV_DOP_TOP type FLAG
      !ET_HISTORY type CDREDCD_TAB
      !ES_UI_KPI_GRA type ZHR_UI_KPI_GRA_S .
  class-methods GET_TIMELINE_SET
    importing
      !IM_REQID type ZHRE_UI_REQID
      !IM_UNAME type UNAME
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
      !IM_INITS_FL type FLAG optional
    changing
      value(CH_SET) type ZCL_ZHR_KPI_APP_MPC=>TT_TIMELINE .
  class-methods GET_APPR_ROUTE_ALL
    importing
      !IM_AUTHOR type UNAME
      !IV_NOSEND type FLAG optional
      !IV_YEAR type JAHR optional
    changing
      !CH_TAB type ZHR_UI_APPROVER_T_ALL .
  class-methods GET_APPR_ROUTE
    importing
      !IM_AUTHOR type UNAME
      !IV_NOSEND type FLAG optional
      !IV_YEAR type JAHR optional
    changing
      !CH_TAB type ZHR_UI_APPROVER_T .

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
    redefinition .
protected section.

  methods APPROVERSET_GET_ENTITYSET
    redefinition .
  methods DELEGSET_GET_ENTITYSET
    redefinition .
  methods FILESET_GET_ENTITY
    redefinition .
  methods GOALSET_GET_ENTITYSET
    redefinition .
  methods GROUPSET_GET_ENTITYSET
    redefinition .
  methods LINESET_GET_ENTITYSET
    redefinition .
  methods PERSONSET_GET_ENTITY
    redefinition .
  methods PERSONSET_GET_ENTITYSET
    redefinition .
  methods SHORGEHSET_GET_ENTITYSET
    redefinition .
  methods SHPERSSET_GET_ENTITYSET
    redefinition .
  methods SHQUEUESET_GET_ENTITYSET
    redefinition .
  methods SHYEARSET_GET_ENTITYSET
    redefinition .
  methods STATUSSET_GET_ENTITYSET
    redefinition .
  methods TIMELINESET_GET_ENTITYSET
    redefinition .
  methods USERSET_GET_ENTITY
    redefinition .
  methods YEARSSET_GET_ENTITYSET
    redefinition .
private section.

  constants C_REPPR_PREM type HR_REP_PRF value 'ZPREM' ##NO_TEXT.
  constants C_OTYPE_P type OTYPE value 'P' ##NO_TEXT.
  constants C_TDID type THEAD-TDID value 'ZKPI' ##NO_TEXT.
  constants C_OBJECT type THEAD-TDOBJECT value 'Z_FIO_KPI' ##NO_TEXT.
  constants C_OTYPE_S type OTYPE value 'S' ##NO_TEXT.
  constants C_PLVAR type PLVAR value '01' ##NO_TEXT.
  constants C_OTYPE_O type OTYPE value 'O' ##NO_TEXT.
  constants C_WEGID_OSP type WEGID value 'O-S-P' ##NO_TEXT.
  constants C_WEGID_O type WEGID value 'O-O_DOWN' ##NO_TEXT.
  constants C_OBJ_CLASS type CDOBJECTCL value 'ZHRT_UI_KPI_GRA' ##NO_TEXT.
  constants C_SES type SUBTY value '9009' ##NO_TEXT.
  constants C_MAIL type SUBTY value '0010' ##NO_TEXT.
  constants C_ST_99 type ZHR_IDENT_ST value '99' ##NO_TEXT.
  constants C_ST_02 type ZHR_IDENT_ST value '02' ##NO_TEXT.
  constants C_ST_03 type ZHR_IDENT_ST value '03' ##NO_TEXT.
  constants C_ST_01 type ZHR_IDENT_ST value '01' ##NO_TEXT.
  constants C_ST_04 type ZHR_IDENT_ST value '04' ##NO_TEXT.
  constants C_ST_05 type ZHR_IDENT_ST value '05' ##NO_TEXT.
  constants C_ST_06 type ZHR_IDENT_ST value '06' ##NO_TEXT.
  constants C_ST_11 type ZHR_IDENT_ST value '11' ##NO_TEXT.
  constants C_9009 type SUBTY value '9009' ##NO_TEXT.
  constants C_WEGID_UP type WEGID value 'ORGA-UP' ##NO_TEXT.
  constants C_QUEUE_BEFORE type ZHRE_UI_QUEUE value '1' ##NO_TEXT.
  constants C_QUEUE_AFTER type ZHRE_UI_QUEUE value '2' ##NO_TEXT.
  constants C_SUBTY_0105 type SUBTY value '0001' ##NO_TEXT.

  methods GET_DATA_KPI
    importing
      !IM_REQID_STR type ZHRE_UI_REQID
      !IM_YEAR type GJAHR
    returning
      value(RV_DATA_KPI) type ZCL_ZHR_KPI_APP_MPC=>TS_PERSON .
  methods GET_REPOSITORY
    returning
      value(RV_REPOSITORY) type SAEARCHIVI
    raising
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GET_LEADER
    importing
      !IM_UNAME type UNAME
      !IM_AUTHOR type UNAME optional
    returning
      value(RET_UNAME) type UNAME .
  class-methods SEND_NOSES
    importing
      !IMPER type PERSNO .
  class-methods GEN_URL
    returning
      value(URL) type STRING .
  methods CHECK_ADMIN
    importing
      !IM_UNAME type UNAME
    returning
      value(FLAG_TOP) type FLAG .
  class-methods CHECK_TOP_MAN
    importing
      !IM_UNAME type UNAME
      !IM_DATE type ZHRE_UI_REQ_DATE optional
    returning
      value(FLAG_TOP) type FLAG .
  methods MULTIPLEAPPROVE
    importing
      !IM_REQID_STR type STRING
      !IM_UNAME type UNAME
    exporting
      !EX_FLAG_OK type FLAG .
  methods MULTIPLESUBMIT
    importing
      !IM_REQID_STR type STRING
      !IM_UNAME type UNAME
    exporting
      !EX_FLAG_OK type FLAG .
  methods GET_DELEG_TAB
    importing
      !IM_UNAME type UNAME
    changing
      !CH_UNAME_TAB type ZHR_UNAME_T .
  methods GET_OE
    importing
      !IM_PLANS type PLANS
    returning
      value(RET_ORGEH) type ORGEH .
  methods WRITE_TEXT
    exporting
      !TEXT_ID type THEAD-TDNAME
      !TEXT type ZHRE_UI_COMMENT_LONG .
  methods GET_PERSON_SET
    importing
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET
      !IM_ADM_FLAG type FLAG optional
    changing
      !CH_SET type ZCL_ZHR_KPI_APP_MPC=>TT_PERSON optional .
  methods GET_OLD_APPROVER
    importing
      !IM_UNAME type UNAME
      !IM_REQID type ZHRE_UI_REQID
    returning
      value(RET_UNAME) type UNAME .
  methods GET_OE_ADMIN
    importing
      !IM_ORGEH type ORGEH
    exporting
      !EX_PERNR type PERSNO
      !EX_ORGEH type ORGEH .
  methods GET_YEARS_SET
    importing
      !IM_UNAME type UNAME
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
      !IM_INITS_FL type FLAG optional
    changing
      value(CH_SET) type ZCL_ZHR_KPI_APP_MPC=>TT_YEARS .
  methods WRITE_APPROVERSET
    changing
      !CH_ENTITY type ZCL_ZHR_KPI_APP_MPC=>TS_USER .
  methods CHANGE_TABLE_GRA
    importing
      !IM_OPERA type CHAR4
      !IM_ORGEH_LEAD type ORGEH
    changing
      !CH_TAB type ZHR_UI_APPROVER_DB_T .
ENDCLASS.



CLASS ZCL_ZHR_KPI_APP_DPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ZHR_KPI_APP_DPC_EXT->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING(optional)
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING(optional)
* | [--->] IV_SOURCE_NAME                 TYPE        STRING(optional)
* | [--->] IO_DATA_PROVIDER               TYPE REF TO /IWBEP/IF_MGW_ENTRY_PROVIDER
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH(optional)
* | [--->] IO_EXPAND                      TYPE REF TO /IWBEP/IF_MGW_ODATA_EXPAND
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY_C(optional)
* | [<---] ER_DEEP_ENTITY                 TYPE REF TO DATA
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

    DATA: ls_ent              TYPE zcl_zhr_kpi_app_mpc=>ts_person,
          ls_old_head_request TYPE zhrt_ui_kpi_gra,
          ls_new_head_request TYPE zhrt_ui_kpi_gra,
          ls_goal             TYPE zhrt_ui_kpi,
          lt_goal             TYPE TABLE OF zhrt_ui_kpi,
          lv_thead            TYPE thead,
          it_lines            TYPE tttext,
          l_textname          TYPE thead-tdname,
          lt_text             TYPE TABLE OF pru_char255,
          l_type_op           TYPE cdpos-chngind VALUE 'I',
          l_user              TYPE uname,
          lt_deleg            TYPE zhr_uname_t,
          l_start             TYPE uname,
          l_collapsed         TYPE zhre_ui_collapsed_text,
          l_text              TYPE zhre_ui_comment_long,
          lt_route            TYPE zhr_ui_approver_t,
*          lt_route_double     TYPE zhr_ui_approver_t,
          lt_request          TYPE zhr_ui_kpi_gra_tt,
          lv_ses_pernr        TYPE pernr_d.

    SELECT * FROM zhrt_ui_leave_st INTO TABLE @DATA(lt_status).

* Смотрим имя сета
    CASE io_tech_request_context->get_entity_set_name( ).
      WHEN 'PersonSet'.
        io_data_provider->read_entry_data( IMPORTING es_data = ls_ent ).
        l_collapsed = ls_ent-collapsed.
        l_user = ls_ent-delegate.
        IF l_user IS INITIAL.
          l_user = sy-uname.
        ENDIF.

        MOVE-CORRESPONDING ls_ent TO ls_new_head_request.
        SELECT SINGLE *
          FROM zhrt_ui_kpi_gra
          INTO @ls_old_head_request
          WHERE reqid = @ls_ent-reqid
            AND uname = @ls_new_head_request-uname
            AND yeara = @ls_ent-year.
        IF sy-subrc NE 0.
          l_type_op = 'I'.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'ZHRFIOKPI'
            IMPORTING
              number                  = ls_new_head_request-reqid
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.
          ls_ent-reqid = ls_new_head_request-reqid.
          DATA(lf_new) = abap_true.
        ELSE.
          l_type_op = 'U'.
        ENDIF.
        IF lf_new IS NOT INITIAL.
          ls_new_head_request-grade1 = '2'.
          ls_new_head_request-grade2 = '2'.
          ls_new_head_request-procent = '100'.
        ENDIF.
        ls_new_head_request-uname = ls_ent-uname.
*        ls_new_head_request-aenam = sy-uname.
        ls_new_head_request-aenam = l_user.
        IF ls_ent-delegate IS NOT INITIAL.
          ls_new_head_request-deleg = sy-uname.   " ins NaumovSM 16.12.20
        ENDIF.
        ls_new_head_request-yeara = ls_ent-year+0(4).
        ls_new_head_request-rdate = sy-datum.
        ls_new_head_request-rtime = sy-uzeit.

* Получаем полный маршрут согласования
        CLEAR: lv_ses_pernr.
****        get_appr_route( EXPORTING
****                          im_author = ls_new_head_request-uname
****                          iv_year = ls_new_head_request-yeara
****                        CHANGING
****                          ch_tab = lt_route ).
* <-- ins NaumovSM 14.12.20 7700113464
        CALL METHOD zcl_zhr_kpi_app_dpc_ext=>get_history
          EXPORTING
            iv_reqid   = ls_new_head_request-reqid
            iv_uname   = ls_new_head_request-uname
            iv_year    = ls_new_head_request-yeara
*           iv_del     = 'X'
          IMPORTING
            et_route   = lt_route
            et_request = lt_request.

        IF lt_route IS NOT INITIAL.
          READ TABLE lt_route WITH KEY top = abap_true ASSIGNING FIELD-SYMBOL(<top_route>).
          IF sy-subrc EQ 0.
            IF <top_route>-orgeh IN zcl_tvarvc=>read( i_name = 'ZHR_FIO_GRA_TOP' i_type = 'S' ).
              DATA(lv_top_dop) = lt_route[ ( lines( lt_route ) - 1 ) ]-uname.
              DELETE lt_route INDEX ( lines( lt_route ) ).
            ELSE.
              IF <top_route>-no_ses = abap_true.
                lv_ses_pernr = <top_route>-pernr.
              ENDIF.
            ENDIF.
          ELSE.
            lv_top_dop   = lt_route[ lines( lt_route ) ]-uname.
            IF lt_route[ lines( lt_route ) ]-no_ses = abap_true.
              lv_ses_pernr = lt_route[ lines( lt_route ) ]-pernr.
            ENDIF.
          ENDIF.
        ENDIF.
* --> ins NaumovSM 14.12.20 7700113464

*        IF l_user NE sy-uname.
*          get_deleg_tab( EXPORTING im_uname = sy-uname
*                          CHANGING ch_uname_tab = lt_deleg ).
*          CLEAR l_start.
*          READ TABLE lt_deleg WITH KEY table_line = l_user INTO l_start.
*          ls_new_head_request-approver = get_leader( im_uname = l_start ).
*        ELSE.
*          ls_new_head_request-approver = get_leader( im_uname = l_user ).
*        ENDIF.

*        IF ls_new_head_request-status <> '04' AND ls_new_head_request-status <> '99'.    " только для отклонения
*          CLEAR: ls_new_head_request-declinecommentary
*               , ls_new_head_request-percentcommentary
*               .
*        ENDIF.

        CASE ls_new_head_request-status.
          WHEN '99'. "Сохранить для руководителя
*            IF check_top_man( im_uname = l_user ) = abap_true OR lv_top_dop = l_user.   " ins NaumovSM 18.12.20
*              CLEAR ls_new_head_request-approver.
*            ELSE.
            ls_new_head_request-approver = l_user.
*            ENDIF.
            ls_new_head_request-status = c_st_02.
            ls_new_head_request-operation = 1.
*        send_noses( imper = ls_lead-pernr ).
          WHEN '04'. "Отклонить
****            READ TABLE lt_route WITH KEY uname = l_user ASSIGNING FIELD-SYMBOL(<route>).
***** <-- naumovsm 7700113464 14.12.2020
****            IF l_user = ls_new_head_request-uname AND sy-subrc = 0.
****              IF <route>-uname = l_user.
****                READ TABLE lt_route WITH KEY uname = l_user
****                                             add_fl = abap_true ASSIGNING <route>.
****                IF sy-subrc = 0.
****                  DATA(lv_add_fl) = abap_true.
****                ENDIF.
****              ELSE.
****                READ TABLE lt_route WITH KEY uname = l_user ASSIGNING <route>.
****              ENDIF.
****            ENDIF.
* { ins NaumovSM считываем корректную запись из маршрута
            DATA(ls_route_04) = zcl_zhr_kpi_app_dpc_ext=>get_actual_route_line(
                                            it_route            = lt_route
                                            is_new_head_request = ls_new_head_request
                                            is_old_head_request = ls_old_head_request
                                            iv_user             = l_user
                                            iv_way              = 'B'        " назад
                                            it_request          = lt_request ).
            IF ls_route_04 IS NOT INITIAL.

* { ins NaumovSM
* если актуальная запись отклонена ищем предыдущую неотклонённую запись по данному маршруту
              IF ls_new_head_request-declinecommentary IS NOT INITIAL.
                DO lines( lt_request ) TIMES.
                  DATA(lv_actual_idx) = lines( lt_request ) - sy-index + 1.
* считываем предыдущую не отклонённую запись с предыдущим индексом маршрута
                  LOOP AT lt_request FROM lv_actual_idx ASSIGNING FIELD-SYMBOL(<ls_actual_req>)
                                                            WHERE declinecommentary IS INITIAL
                                                              AND idx = ( ls_route_04-idx - 2 ).   " т.к. выбранная ранее это следующая, нужна предыдущая текущей
                    EXIT.
                  ENDLOOP.
                  IF sy-subrc = 0.
                    DATA(lv_actual_approver) = <ls_actual_req>-aenam.
                    EXIT.
                  ENDIF.
                ENDDO.
              ENDIF.
              IF lv_actual_approver IS NOT INITIAL.
                ls_new_head_request-approver = lv_actual_approver.
              ELSE.
                ls_new_head_request-approver = ls_route_04-uname.
                DATA(lv_add_fl)              = ls_route_04-add_fl.
              ENDIF.
* } ins NaumovSM

****              DATA(l_idx)                  = ls_route_04-idx - 1.
*              ls_new_head_request-approver = ls_route_04-uname.
* } ins NaumovSM считываем корректную запись из маршрута
* --> NaumovSM 7700113464 14.12.2020
****            IF sy-subrc EQ 0.
****              DATA(l_idx) = sy-tabix - 1.
****              TRY.
********                  READ TABLE lt_route INDEX l_idx ASSIGNING <route>.
****                  READ TABLE lt_route WITH KEY idx = l_idx ASSIGNING FIELD-SYMBOL(<route>).
****                  IF sy-subrc EQ 0.
****                    ls_new_head_request-approver = <route>-uname.
****                  ENDIF.
****                CATCH cx_root.
****              ENDTRY.
            ENDIF.
            IF ls_new_head_request-approver EQ ls_new_head_request-uname.
              ls_new_head_request-status = c_st_04.
            ELSE.
              ls_new_head_request-status = c_st_02.
            ENDIF.
            ls_new_head_request-operation = 4.

*            ls_new_head_request-approver = l_user.
          WHEN '03'. "Согласовать
            ls_new_head_request-status = c_st_02.
*            READ TABLE lt_route WITH KEY uname = l_user ASSIGNING <route>.   " del NaumovSM
* { ins NaumovSM
* проверяем предыдущую запись по маршруту на аппрувера, чтобы идентифицировать текущую (при делегатах м.б. несколько одинаковых логинов)
* также читаем следующую запись по марштуру для исключения неправильного согласования при нескольких логинах.
****         if sy-subrc = 0.
****            LOOP AT lt_route ASSIGNING <route> WHERE uname = l_user.
****              DATA(lv_route_idx) = sy-tabix.
****              DATA(lv_prev_idx) = sy-tabix - 1.
****              DATA(lv_next_idx) = sy-tabix + 1.
****              CHECK lv_prev_idx > 0.
****              READ TABLE lt_route INDEX lv_prev_idx ASSIGNING FIELD-SYMBOL(<ls_prev_route>).
****              IF sy-subrc = 0.
***** Проверяем что запись не была позднее отклонена
****                DATA(lv_next_request_idx) = <route>-idx + 1.
****                READ TABLE lt_request WITH KEY idx = lv_next_request_idx ASSIGNING FIELD-SYMBOL(<ls_request>).
****                IF sy-subrc = 0.
****                  IF <ls_request>-declinecommentary IS NOT INITIAL.
****                    IF ls_new_head_request-aenam = <ls_request>-approver OR
****                       ls_new_head_request-deleg = <ls_request>-approver.
****
****                      READ TABLE lt_route INDEX lv_next_idx ASSIGNING FIELD-SYMBOL(<ls_next_route>).
****                      IF sy-subrc = 0.
****                        DATA(lv_next_uname) = <ls_next_route>-uname.
****                      ENDIF.
****
****                      EXIT.
****                    ENDIF.
****                  ENDIF.
****                ELSE.
****                  IF <ls_prev_route>-uname = ls_old_head_request-aenam OR
****                     <ls_prev_route>-uname = ls_old_head_request-deleg.
****
****                    READ TABLE lt_route INDEX lv_next_idx ASSIGNING <ls_next_route>.
****                    IF sy-subrc = 0.
****                      lv_next_uname = <ls_next_route>-uname.
****                    ENDIF.
****
****                    EXIT.
****                  ELSE.
****                    CONTINUE.
****                  ENDIF.
****                ENDIF.
****              ENDIF.
****            ENDLOOP.
***** } ins NaumovSM
***** <-- NaumovSM 7700113464 14.12.2020
****            IF l_user = ls_new_head_request-uname AND sy-subrc = 0.
****              IF <route>-uname = l_user.
****                READ TABLE lt_route WITH KEY uname = l_user
****                                               idx = lv_route_idx
****                                            add_fl = abap_true ASSIGNING <route>.
****                IF sy-subrc = 0.
****                  lv_add_fl = abap_true.
****                ENDIF.
****              ENDIF.
****            ENDIF.
***** --> NaumovSM 7700113464 14.12.2020
****            IF sy-subrc EQ 0.
*****              l_idx = sy-tabix + 1.   " del NaumovSM
****              TRY.
*****                  READ TABLE lt_route INDEX l_idx ASSIGNING FIELD-SYMBOL(<lv_next_route>).   " del NaumovSM
****                  READ TABLE lt_route INDEX lv_next_idx ASSIGNING FIELD-SYMBOL(<lv_next_route>).   " ins NaumovSM
****                  IF sy-subrc EQ 0.
****                    ls_new_head_request-approver = <lv_next_route>-uname.
****                  ENDIF.
****                CATCH cx_root.
****              ENDTRY.
****            ENDIF.
* { ins NaumovSM считываем корректную запись из маршрута
            DATA(ls_route_03) = zcl_zhr_kpi_app_dpc_ext=>get_actual_route_line(
                                            it_route            = lt_route
                                            is_new_head_request = ls_new_head_request
                                            is_old_head_request = ls_old_head_request
                                            iv_user             = l_user
                                            iv_way              = 'F'
                                            it_request          = lt_request ).
            IF ls_route_03 IS NOT INITIAL.
              ls_new_head_request-approver = ls_route_03-uname.
              lv_add_fl                    = ls_route_03-add_fl.
* } ins NaumovSM считываем корректную запись из маршрута
*            IF check_top_man( im_uname = l_user ) = abap_true.   " del NaumovSM 18.12.20
*            IF check_top_man( im_uname = l_user ) = abap_true OR lv_top_dop = l_user.   " ins NaumovSM 18.12.20
              IF ( check_top_man( im_uname = l_user ) = abap_true OR lv_top_dop = l_user ) AND
                lt_route[ lines( lt_route ) ]-idx  = ls_route_03-idx.      " последняя в маршруте
*                 ls_new_head_request-approver IS INITIAL.
                ls_new_head_request-status = c_st_03.
*              ls_new_head_request-approver = l_user.
                CLEAR ls_new_head_request-approver.
              ENDIF.
            ENDIF.
            ls_new_head_request-operation = 3.
          WHEN '01'. "Сохранить для работника
            ls_new_head_request-approver = l_user.
            ls_new_head_request-operation = 1.
          WHEN '02'. "Согласовать
****            READ TABLE lt_route WITH KEY uname = l_user ASSIGNING <route>.
***** <-- naumovsm 7700113464 14.12.2020
****            IF l_user = ls_new_head_request-uname AND sy-subrc = 0.
****              IF <route>-uname = l_user.
****                READ TABLE lt_route WITH KEY uname = l_user
****                                             add_fl = abap_false ASSIGNING <route>.
****                IF sy-subrc <> 0.
****                  lv_add_fl = abap_true.
****                ENDIF.
****              ELSE.
****                READ TABLE lt_route WITH KEY uname = l_user ASSIGNING <route>.
****              ENDIF.
****            ENDIF.
***** --> NaumovSM 7700113464 14.12.2020
****            IF sy-subrc EQ 0.
****              l_idx = sy-tabix + 1.
****              TRY.
****                  READ TABLE lt_route INDEX l_idx ASSIGNING <route>.
****                  IF sy-subrc EQ 0.
****                    ls_new_head_request-approver = <route>-uname.
****                  ENDIF.
****                CATCH cx_root.
****              ENDTRY.
****            ENDIF.

* { ins NaumovSM считываем корректную запись из маршрута
            DATA(ls_route_02) = zcl_zhr_kpi_app_dpc_ext=>get_actual_route_line(
                                            it_route            = lt_route
                                            is_new_head_request = ls_new_head_request
                                            is_old_head_request = ls_old_head_request
                                            iv_user             = l_user
                                            iv_way              = 'F'
                                            it_request          = lt_request ).
            IF ls_route_02 IS NOT INITIAL.
              ls_new_head_request-approver = ls_route_02-uname.
              lv_add_fl                    = ls_route_02-add_fl.
* } ins NaumovSM считываем корректную запись из маршрута

*            IF check_top_man( im_uname = l_user ) = abap_true.   " del NaumovSM 18.12.20
              IF check_top_man( im_uname = l_user ) = abap_true OR lv_top_dop = l_user.   " ins NaumovSM 18.12.20
                ls_new_head_request-status = c_st_03.
                ls_new_head_request-approver = ls_route_02-uname.
*              CLEAR ls_new_head_request-approver.
              ENDIF.
              IF ls_new_head_request-approver EQ ls_new_head_request-uname.
                ls_new_head_request-status = c_st_01.
              ENDIF.
            ENDIF.

* { ins NaumovSM
            IF lv_ses_pernr IS NOT INITIAL AND
               ls_new_head_request-status = c_st_02 AND
              ( ls_old_head_request-status = c_st_99 OR
                lf_new = abap_true ).
*              send_noses( imper = lv_ses_pernr ).
            ENDIF.
* } ins NaumovSM
            ls_new_head_request-operation = 2.
          WHEN '98'. "Принять
            IF check_admin( im_uname = l_user ) = abap_true.
              ls_new_head_request-status = c_st_06.
              CLEAR ls_new_head_request-approver.
            ENDIF.
            ls_new_head_request-operation = 6.
          WHEN '97'. "Вернуть
            IF check_admin( im_uname = l_user ) = abap_true.
              ls_new_head_request-status = c_st_03.
              DESCRIBE TABLE lt_route LINES DATA(l_idx).
              READ TABLE lt_route INDEX l_idx ASSIGNING FIELD-SYMBOL(<route>).
              IF sy-subrc EQ 0.
                ls_new_head_request-approver = <route>-uname.
              ENDIF.
*              ls_new_head_request-approver = get_old_approver( im_uname = ls_new_head_request-uname im_reqid = ls_new_head_request-reqid ).
            ENDIF.
            ls_new_head_request-operation = 5.
          WHEN OTHERS.

        ENDCASE.
* Пишем в базу
        CONDENSE ls_new_head_request-procent NO-GAPS.
        l_textname = |{ ls_new_head_request-reqid }D{ ls_new_head_request-rdate }{ ls_new_head_request-rtime }|.
*        l_text = ls_ent-declinecommentary.   " del NaumovSM
        l_text = ls_new_head_request-declinecommentary.   " ins NaumovSM
        CALL METHOD write_text(
          IMPORTING
            text_id = l_textname
            text    = l_text ).
        CLEAR l_text.
        l_textname = |{ ls_new_head_request-reqid }P{ ls_new_head_request-rdate }{ ls_new_head_request-rtime }|.
*        l_text = ls_ent-percentcommentary.   " del NaumovSM
        l_text = ls_new_head_request-percentcommentary.     " isn NaumovSM
        CALL METHOD write_text(
          IMPORTING
            text_id = l_textname
            text    = l_text ).
        CLEAR: ls_new_head_request-declinecommentary, ls_new_head_request-percentcommentary.

        IF sy-subrc EQ 0.
          MODIFY zhrt_ui_kpi_gra FROM ls_new_head_request.
        ENDIF.

        TRY.
            CALL FUNCTION 'ZHRT_UI_KPI_GRA_WRITE_DOCUMENT'
              EXPORTING
                objectid            = CONV cdobjectv( ls_new_head_request-reqid )
                tcode               = sy-tcode
                utime               = sy-uzeit
                udate               = sy-datum
                username            = sy-uname
                n_zhrt_ui_kpi_gra   = ls_new_head_request
                o_zhrt_ui_kpi_gra   = ls_old_head_request
                upd_zhrt_ui_kpi_gra = l_type_op.
          CATCH cx_root INTO DATA(lo_error).
            DATA(lv_dummy) = lo_error->get_text( ).
        ENDTRY.

        SELECT * FROM zhrt_ui_kpi INTO TABLE @DATA(lt_goals_old) WHERE reqid = @ls_ent-reqid.
        LOOP AT ls_ent-togoals ASSIGNING FIELD-SYMBOL(<tog>).
          IF <tog>-text IS NOT INITIAL.
            DELETE lt_goals_old WHERE purposeid = <tog>-purposeid.
          ENDIF.
          CLEAR: l_textname, lt_text[], ls_goal.
          IF <tog>-purposeid IS INITIAL.
            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                nr_range_nr             = '01'
                object                  = 'ZHRKPIGOAL'
              IMPORTING
                number                  = ls_goal-purposeid
              EXCEPTIONS
                interval_not_found      = 1
                number_range_not_intern = 2
                object_not_found        = 3
                quantity_is_0           = 4
                quantity_is_not_1       = 5
                interval_overflow       = 6
                buffer_overflow         = 7
                OTHERS                  = 8.
          ELSE.
            ls_goal-purposeid = <tog>-purposeid.
          ENDIF.
          ls_goal-reqid = ls_new_head_request-reqid.
          CLEAR: l_textname.
          l_textname = |{ ls_goal-reqid }{ ls_new_head_request-uname }{ ls_goal-purposeid }|.

          l_text = <tog>-text.
          CALL METHOD write_text(
            IMPORTING
              text_id = l_textname
              text    = l_text ).

          MOVE-CORRESPONDING ls_goal TO <tog>.
          APPEND ls_goal TO lt_goal.
        ENDLOOP.
        LOOP AT lt_goals_old ASSIGNING FIELD-SYMBOL(<goal>).
          l_textname = |{ <goal>-reqid }{ ls_new_head_request-uname }{ <goal>-purposeid }|.
          CALL FUNCTION 'DELETE_TEXT'
            EXPORTING
*             CLIENT    = SY-MANDT
              id        = 'ZKPI'
              language  = sy-langu
              name      = l_textname
              object    = 'Z_FIO_KPI'
*             SAVEMODE_DIRECT       = ' '
*             TEXTMEMORY_ONLY       = ' '
*             LOCAL_CAT = ' '
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2.
          IF sy-subrc EQ 0.
            CALL FUNCTION 'COMMIT_TEXT'.
            DELETE zhrt_ui_kpi FROM <goal>.
            CONTINUE.
          ENDIF.
        ENDLOOP.
        IF lt_goal[] IS NOT INITIAL.
          MODIFY zhrt_ui_kpi FROM TABLE lt_goal.
        ENDIF.
* Возвращаем результат с учетом изменений
        MOVE-CORRESPONDING ls_new_head_request TO ls_ent.
        CLEAR: ls_ent-crossflag, ls_ent-accessback, ls_ent-accessappr.
*        IF ls_new_head_request-aenam EQ l_user AND ls_new_head_request-approver NE l_user.   " del NaumovSM 14.12.20
        IF ls_new_head_request-aenam EQ l_user AND ( ls_new_head_request-approver NE l_user OR lv_add_fl = abap_true ).   " ins NaumovSM 14.12.20
          ls_ent-crossflag = abap_true.
          ls_ent-accessback = abap_true.
        ENDIF.
        IF ls_new_head_request-approver EQ l_user.
          ls_ent-accessappr = abap_true.
        ENDIF.
        ls_ent-stnam = VALUE #( lt_status[ ident = ls_ent-status ]-stnam OPTIONAL ).
* Отправка уведомления
        IF ( NOT ( ls_new_head_request-status = ls_old_head_request-status AND ls_old_head_request-status = c_st_01 ) ) AND
           ( NOT ( ls_new_head_request-status = ls_old_head_request-status AND ls_new_head_request-approver = ls_old_head_request-approver ) ).
          CALL FUNCTION 'ZHR_RN_REQUEST_SEND_MAIL'
            IN BACKGROUND TASK
            EXPORTING
              numb     = CONV zhre_ui5_req_numb( ls_ent-reqid )
              flag_kpi = abap_true.
        ENDIF.
        ls_ent-collapsed = l_collapsed.
        copy_data_to_ref( EXPORTING is_data = ls_ent
                          CHANGING  cr_data = er_deep_entity ).
      WHEN 'UserSet'.
        DATA: ls_entity_user TYPE zcl_zhr_kpi_app_mpc=>ts_user.
        io_data_provider->read_entry_data( IMPORTING es_data = ls_entity_user ).
        write_approverset( CHANGING ch_entity = ls_entity_user ).
        copy_data_to_ref( EXPORTING is_data = ls_entity_user
                           CHANGING  cr_data = er_deep_entity ).
      WHEN OTHERS.
        TRY.
            CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~create_deep_entity
              EXPORTING
                iv_entity_name          = iv_entity_name
                iv_entity_set_name      = iv_entity_set_name
                iv_source_name          = iv_source_name
                io_data_provider        = io_data_provider
                it_key_tab              = it_key_tab
                it_navigation_path      = it_navigation_path
                io_expand               = io_expand
                io_tech_request_context = io_tech_request_context
              IMPORTING
                er_deep_entity          = er_deep_entity.
          CATCH /iwbep/cx_mgw_busi_exception .
          CATCH /iwbep/cx_mgw_tech_exception .
        ENDTRY.
    ENDCASE.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ZHR_KPI_APP_DPC_EXT->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ACTION_NAME                 TYPE        STRING(optional)
* | [--->] IT_PARAMETER                   TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_FUNC_IMPORT(optional)
* | [<---] ER_DATA                        TYPE REF TO DATA
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.
    CASE io_tech_request_context->get_function_import_name( ).
      WHEN 'MultipleApprove'.
        DATA: l_str   TYPE string,
              l_uname TYPE uname.
        DATA(lt_key) = io_tech_request_context->get_parameters( ).
*        READ TABLE lt_key INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_key>).
        l_str = VALUE #( lt_key[ name = 'REQID' ]-value OPTIONAL ).
        l_uname = VALUE #( lt_key[ name = 'DELEGATE' ]-value OPTIONAL ).
        IF l_uname IS INITIAL.
          l_uname = sy-uname.
        ENDIF.

        CALL METHOD multipleapprove(
          EXPORTING
            im_reqid_str = l_str
            im_uname     = l_uname
          IMPORTING
            ex_flag_ok   = DATA(lf_ok) ).
*      WHEN 'IntWorkResults'.
*        DATA: l_strid  TYPE string.
*        DATA(lt_keyid) = io_tech_request_context->get_parameters( ).
**        READ TABLE lt_key INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_key>).
*        l_strid = VALUE #( lt_keyid[ name = 'REQID' ]-value OPTIONAL ).
*
*        CALL METHOD intworkresults(
*          EXPORTING
*            im_reqid_str = l_strid
*          IMPORTING
*            ex_xstr      = DATA(xstring) ).
*
*        DATA ls_return TYPE zcl_zhr_kpi_app_mpc=>ts_return.
*        ls_return-value = xstring.
*
*        copy_data_to_ref( EXPORTING is_data = ls_return
*                          CHANGING cr_data = er_data ).

* <-- NaumoSM 15.12.2020

      WHEN 'MultipleSubmit'.

        lt_key = io_tech_request_context->get_parameters( ).

        l_str = VALUE #( lt_key[ name = 'REQID' ]-value OPTIONAL ).
        l_uname = VALUE #( lt_key[ name = 'DELEGATE' ]-value OPTIONAL ).
        IF l_uname IS INITIAL.
          l_uname = sy-uname.
        ENDIF.

        CALL METHOD multiplesubmit(
          EXPORTING
            im_reqid_str = l_str
            im_uname     = l_uname
          IMPORTING
            ex_flag_ok   = lf_ok ).

* --> NaumoSM 15.12.2020

      WHEN OTHERS.
        TRY.
            CALL METHOD super->/iwbep/if_mgw_appl_srv_runtime~execute_action
              EXPORTING
                iv_action_name          = iv_action_name
                it_parameter            = it_parameter
                io_tech_request_context = io_tech_request_context
              IMPORTING
                er_data                 = er_data.
          CATCH /iwbep/cx_mgw_busi_exception .
          CATCH /iwbep/cx_mgw_tech_exception .
        ENDTRY.
    ENDCASE.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ZHR_KPI_APP_DPC_EXT->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING(optional)
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING(optional)
* | [--->] IV_SOURCE_NAME                 TYPE        STRING(optional)
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [<---] ER_STREAM                      TYPE REF TO DATA
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /iwbep/if_mgw_appl_srv_runtime~get_stream.
    DATA ls_stream TYPE ty_s_media_resource.
    DATA ls_file TYPE zcl_zedu_abap_ex_mpc=>ts_file.

    CASE io_tech_request_context->get_entity_set_name( ).
      WHEN 'FileSet'.
        DATA(lt_key) = io_tech_request_context->get_keys( ).
*        READ TABLE lt_key WITH KEY name = 'CARRID' ASSIGNING FIELD-SYMBOL(<ls_carrid>).
*        READ TABLE lt_key WITH KEY name = 'FILENAME' ASSIGNING FIELD-SYMBOL(<ls_name>).
*
*        SELECT SINGLE *
*          FROM zedu_ex_files_gy
*          INTO @ls_file
*          WHERE carrid = @<ls_carrid>-value AND filename = @<ls_name>-value.
*
*        IF sy-subrc IS INITIAL.
*          ls_stream-mime_type = ls_file-mime_type.
*          ls_stream-value = ls_file-value.
*          copy_data_to_ref( EXPORTING is_data = ls_stream
*                            CHANGING cr_data = er_stream ).
*
*        ENDIF.
*******************************************
        DATA: lv_pdf         TYPE fpcontent,
              lv_length      TYPE i,
              lt_binary      TYPE TABLE OF x,
              lv_docid       TYPE saeardoid,
              lv_uri         TYPE saeuri,
              l_trans        TYPE string,
              lv_par         TYPE i,
              tfill          TYPE i,
              ls_msg         TYPE string,
              l_compsd       TYPE TABLE OF scms_doinf,
              l_compsn       TYPE TABLE OF scms_donam,
              lv_xml_content TYPE xstring.

        DATA: absolute_uri        TYPE saeuri,
              http_uri            TYPE saeuri,
              https_uri           TYPE saeuri,
              eostr_signed_data_l TYPE ssfparms-sigdatalen,
              cache               TYPE scmscache,
              ls_data_kpi         TYPE zcl_zhr_kpi_app_mpc=>ts_person,
*              ls_full_data_kpi    TYPE zhr_ui_kpi_person_full_data_s,
              ls_lheader          TYPE ihttpnvp,
              lv_filename         TYPE string,
              lv_data             TYPE string,
              lv_time             TYPE string,
              l_reqid             TYPE zhre_ui_reqid,
              l_year              TYPE gjahr.

*        IF sy-subrc = 0.
*
*          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*            EXPORTING
*              buffer        = lv_xml_content
*            IMPORTING
*              output_length = lv_length
*              buffer        = ls_stream-value
*            TABLES
*              binary_tab    = lt_binary.
*        ENDIF.

        l_reqid = lt_key[ name = 'REQID' ]-value.
        l_year = lt_key[ name = 'YEAR' ]-value.
        ls_data_kpi = get_data_kpi( EXPORTING im_reqid_str    = l_reqid
                                              im_year         = l_year ).   " ins NaumovSM
        l_trans = 'ZHR_IND_WORK_RESULTS'."'ZHR_ORDER_ZEGV'."'ZHR_IND_WORK_RESULTS'.
        CLEAR: lv_xml_content.
        CALL TRANSFORMATION (l_trans)
        SOURCE
        table      = ls_data_kpi
        RESULT XML lv_xml_content.
        ls_stream-value = lv_xml_content.
        ls_stream-mime_type = 'application/msword'."lv_mimetype.
        lv_data = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
        lv_time = |{ sy-uzeit(2) }:{ sy-uzeit+2(2) }:{ sy-uzeit+4(2) }|.
        lv_filename = escape( val = lv_filename format = cl_abap_format=>e_url ).
        ls_lheader-name = 'Content-Disposition'.
        ls_lheader-value = 'outline; filename="'.

        CONCATENATE ls_lheader-value ls_data_kpi-uname'(' ls_data_kpi-year ')_' lv_data',' lv_time'.doc' INTO ls_lheader-value.
*        CONCATENATE ls_lheader-value 'Оценка индивидуальных рабочих результатов за ' ls_data_kpi-year 'для целей годового премирования.doc' INTO ls_lheader-value.
        CONCATENATE ls_lheader-value '"' INTO ls_lheader-value.
        set_header( is_header = ls_lheader ).
        copy_data_to_ref( EXPORTING is_data = ls_stream
                          CHANGING cr_data = er_stream ).

      WHEN OTHERS.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
**  EXPORTING
**    iv_entity_name          =
**    iv_entity_set_name      =
**    iv_source_name          =
**    it_key_tab              =
**    it_navigation_path      =
**    io_tech_request_context =
**  IMPORTING
**    er_stream               =
**    es_response_context     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
    ENDCASE.


**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_STREAM
**  EXPORTING
**    iv_entity_name          =
**    iv_entity_set_name      =
**    iv_source_name          =
**    it_key_tab              =
**    it_navigation_path      =
**    io_tech_request_context =
**  IMPORTING
**    er_stream               =
**    es_response_context     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->APPROVERSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_APPROVER
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD approverset_get_entityset.
    DATA: _bipernr     TYPE REF TO zhr_objbif_pernr,
          _biorgeh     TYPE REF TO zhr_objbif_orgeh,
          l_pernr      TYPE persno,
          lt_p0002     TYPE TABLE OF p0002,
          l_uname      TYPE uname,
          l_cond_nachn TYPE string,
          l_cond_vorna TYPE string,
          l_cond_midnm TYPE string,
          l_year       TYPE jahr,
          lt_appr      TYPE TABLE OF zhrt_ui_approver.

    DATA(lt_filter) = io_tech_request_context->get_filter( )->get_filter_select_options( ).
    TRY.
        DATA(r_nachn) = lt_filter[ property = 'NACHN' ]-select_options.
        l_cond_nachn = |*{ r_nachn[ 1 ]-low }*|.
      CATCH cx_root.
        l_cond_nachn = |*|.
    ENDTRY.
    TRY.
        DATA(r_vorna) = lt_filter[ property = 'VORNA' ]-select_options.
        l_cond_vorna = |*{ r_vorna[ 1 ]-low }*|.
      CATCH cx_root.
        l_cond_vorna = |*|.
    ENDTRY.
    TRY.
        DATA(r_midnm) = lt_filter[ property = 'MIDNM' ]-select_options.
        l_cond_midnm = |*{ r_midnm[ 1 ]-low }*|.
      CATCH cx_root.
        l_cond_midnm = |*|.
    ENDTRY.
    DATA(lt_keys) = io_tech_request_context->get_source_keys( ).
    l_uname = VALUE #( lt_keys[ name = 'UNAME' ]-value ).
    l_year = VALUE #( lt_keys[ name = 'YEAR' ]-value ).

    SELECT * FROM zhrt_ui_approver INTO TABLE lt_appr WHERE approver = l_uname AND year_appr = l_year .
    SORT lt_appr BY year_appr approver add_approver queue.
    DELETE ADJACENT DUPLICATES FROM lt_appr COMPARING year_appr approver add_approver queue.
    LOOP AT lt_appr ASSIGNING FIELD-SYMBOL(<appr>).
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<entity>).
      MOVE-CORRESPONDING <appr> TO <entity>.
      <entity>-uname = <appr>-add_approver.
      <entity>-year = <appr>-year_appr.
      CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
        EXPORTING
          user_name               = <entity>-uname
        IMPORTING
          employeenumber          = l_pernr
        EXCEPTIONS
          no_employeenumber_found = 1
          subtype_not_available   = 2
          OTHERS                  = 3.
*      m_read_inf l_pernr '0002' lt_p0002.
      m_read_inf l_pernr '0002' lt_p0002 sy-datum.
      READ TABLE lt_p0002 ASSIGNING FIELD-SYMBOL(<p0002>) INDEX 1.
      IF sy-subrc EQ 0.
        <entity>-nachn = <p0002>-nachn.
        <entity>-vorna = <p0002>-vorna.
        <entity>-midnm = <p0002>-midnm.
      ENDIF.
      IF ( NOT <entity>-nachn CP l_cond_nachn ) OR ( <entity>-nachn IS INITIAL AND l_cond_nachn NE '*' ).
        DELETE TABLE et_entityset FROM <entity>.
        CONTINUE.
      ENDIF.
      IF ( NOT <entity>-vorna CP l_cond_vorna ) OR ( <entity>-vorna IS INITIAL AND l_cond_vorna NE '*' ).
        DELETE TABLE et_entityset FROM <entity>.
        CONTINUE.
      ENDIF.
      IF ( NOT <entity>-midnm CP l_cond_midnm ) OR ( <entity>-midnm IS INITIAL AND l_cond_midnm NE '*' ).
        DELETE TABLE et_entityset FROM <entity>.
        CONTINUE.
      ENDIF.
      CHECK l_uname NE sy-uname.
      <entity>-delegate = l_uname.
    ENDLOOP.
    SORT et_entityset BY nachn ASCENDING vorna ASCENDING midnm ASCENDING.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->CHANGE_TABLE_GRA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_OPERA                       TYPE        CHAR4
* | [--->] IM_ORGEH_LEAD                  TYPE        ORGEH
* | [<-->] CH_TAB                         TYPE        ZHR_UI_APPROVER_DB_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD change_table_gra.
    DATA: _struc         TYPE TABLE OF struc,
          r_pernr        TYPE RANGE OF persno,
          lt_old_gra     TYPE TABLE OF zhrt_ui_kpi_gra,
          lt_new_gra     LIKE lt_old_gra,
          l_new_approver TYPE uname,
          l_old_approver TYPE uname,
          l_wegid        TYPE wegid,
          l_year         TYPE jahr.

    LOOP AT ch_tab ASSIGNING FIELD-SYMBOL(<line>)." WHERE orgeh IS NOT INITIAL.
      CLEAR: _struc[], r_pernr, lt_old_gra, lt_new_gra, l_new_approver, l_old_approver.
      IF im_opera EQ 'ADD'.
        l_old_approver = <line>-approver.
        l_new_approver = <line>-add_approver.
      ELSEIF im_opera EQ 'DEL'.
        l_old_approver = <line>-add_approver.
        l_new_approver = <line>-approver.
      ENDIF.
      l_year = <line>-year_appr.
      IF <line>-queue EQ '1'.
        IF <line>-orgeh EQ im_orgeh_lead.
          l_wegid = 'OSP'.
        ELSE.
          l_wegid = 'O-S-P'.
        ENDIF.
        CALL FUNCTION 'RH_STRUC_GET'
          EXPORTING
            act_otype       = c_otype_o
            act_objid       = <line>-orgeh
            act_wegid       = l_wegid
            act_plvar       = c_plvar
            act_begda       = sy-datum
            act_endda       = sy-datum
            authority_check = ''
          TABLES
            result_struc    = _struc
          EXCEPTIONS
            no_plvar_found  = 1
            no_entry_found  = 2
            OTHERS          = 3.
        DELETE _struc WHERE otype NE c_otype_p.
        LOOP AT _struc ASSIGNING FIELD-SYMBOL(<struc>).
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <struc>-objid ) TO r_pernr.
        ENDLOOP.
        "gra~*

*        SELECT zhrt_ui_kpi_gra~reqid
*               zhrt_ui_kpi_gra~uname
*               zhrt_ui_kpi_gra~yeara
*               zhrt_ui_kpi_gra~rdate
*               zhrt_ui_kpi_gra~rtime
*               zhrt_ui_kpi_gra~grade1
*               zhrt_ui_kpi_gra~grade2
*               zhrt_ui_kpi_gra~procent
*               zhrt_ui_kpi_gra~status
*               zhrt_ui_kpi_gra~aenam
*               zhrt_ui_kpi_gra~declinecommentary
*               zhrt_ui_kpi_gra~percentcommentary
*               zhrt_ui_kpi_gra~approver
*          FROM pa0105
*          JOIN zhrt_ui_kpi_gra ON zhrt_ui_kpi_gra~uname = pa0105~usrid
*         INTO TABLE lt_old_gra
**         RIGHT JOIN zhrt_ui_kpi_gra ON zhrt_ui_kpi_gra~uname = pa0105~usrid
*         WHERE pernr IN r_pernr
*           AND pa0105~begda <= sy-datum
*           AND pa0105~endda >= sy-datum
*           AND pa0105~subty = c_subty_0105
*           AND zhrt_ui_kpi_gra~yeara = sy-datum+0(4)
*           AND zhrt_ui_kpi_gra~approver = l_old_approver
*           AND zhrt_ui_kpi_gra~status = c_st_02.
        DATA: lt_pa0105 TYPE TABLE OF pa0105,
              r_us      TYPE RANGE OF uname.
        SELECT * FROM pa0105
          INTO TABLE lt_pa0105
          WHERE pernr IN r_pernr
           AND pa0105~begda <= sy-datum
           AND pa0105~endda >= sy-datum
           AND pa0105~subty = c_subty_0105.
        LOOP AT lt_pa0105 ASSIGNING FIELD-SYMBOL(<pa0105>).
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <pa0105>-usrid ) TO r_us.
        ENDLOOP.
        SORT r_us BY low ASCENDING.
        DELETE ADJACENT DUPLICATES FROM r_us COMPARING low.

*        SELECT zhrt_ui_kpi_gra~reqid
*               zhrt_ui_kpi_gra~uname
*               zhrt_ui_kpi_gra~yeara
*               zhrt_ui_kpi_gra~rdate
*               zhrt_ui_kpi_gra~rtime
*               zhrt_ui_kpi_gra~grade1
*               zhrt_ui_kpi_gra~grade2
*               zhrt_ui_kpi_gra~procent
*               zhrt_ui_kpi_gra~status
*               zhrt_ui_kpi_gra~aenam
*               zhrt_ui_kpi_gra~declinecommentary
*               zhrt_ui_kpi_gra~percentcommentary
*               zhrt_ui_kpi_gra~approver
        SELECT *
          FROM zhrt_ui_kpi_gra
          INTO TABLE lt_old_gra
*         RIGHT JOIN zhrt_ui_kpi_gra ON zhrt_ui_kpi_gra~uname = pa0105~usrid
         WHERE uname IN r_us
           AND yeara = l_year
           AND approver = l_old_approver
           AND status = c_st_02.
      ELSEIF <line>-queue EQ '2'.
        SELECT *
          INTO TABLE lt_old_gra
          FROM zhrt_ui_kpi_gra AS gra
         WHERE gra~yeara = l_year
           AND gra~aenam = l_old_approver
           AND gra~status = c_st_02
           AND gra~approver NE gra~aenam.
      ENDIF.
      lt_new_gra[] = lt_old_gra[].
      LOOP AT lt_new_gra ASSIGNING FIELD-SYMBOL(<new_gra>).
        IF <new_gra>-approver EQ <new_gra>-aenam.
          DELETE lt_old_gra WHERE reqid = <new_gra>-reqid.
          DELETE lt_new_gra WHERE reqid = <new_gra>-reqid.
          CONTINUE.
        ENDIF.
*        <new_gra>-rdate = sy-datum.
*        <new_gra>-rtime = sy-uzeit.
        <new_gra>-approver = l_new_approver.
      ENDLOOP.
      MODIFY zhrt_ui_kpi_gra FROM TABLE lt_new_gra.
      IF sy-subrc EQ 0.
        COMMIT WORK.
        LOOP AT lt_old_gra ASSIGNING FIELD-SYMBOL(<gra>).
          TRY.
              CALL FUNCTION 'ZHRT_UI_KPI_GRA_WRITE_DOCUMENT'
                EXPORTING
                  objectid            = CONV cdobjectv( <gra>-reqid )
                  tcode               = sy-tcode
                  utime               = sy-uzeit
                  udate               = sy-datum
                  username            = sy-uname
                  n_zhrt_ui_kpi_gra   = lt_new_gra[ sy-tabix ]
                  o_zhrt_ui_kpi_gra   = <gra>
                  upd_zhrt_ui_kpi_gra = 'U'.
              CALL FUNCTION 'ZHR_RN_REQUEST_SEND_MAIL'
                IN BACKGROUND TASK
                EXPORTING
                  numb     = CONV zhre_ui5_req_numb( <gra>-reqid )
                  flag_kpi = abap_true.
            CATCH cx_root.
          ENDTRY.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->CHECK_ADMIN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_UNAME                       TYPE        UNAME
* | [<-()] FLAG_TOP                       TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_admin.
    CONSTANTS: c_manager TYPE agr_users-agr_name VALUE '%FIORI_GRADE_ADMIN',
               c_support TYPE agr_users-agr_name VALUE 'ZHR_SUPPORT_BC_CUSTOMDISP'.
    DATA lt_agr TYPE TABLE OF agr_users.
    SELECT * FROM agr_users
      INTO TABLE lt_agr
     WHERE agr_name LIKE c_manager
       AND uname = sy-uname
       AND from_dat <= sy-datum
       AND to_dat >= sy-datum.
    IF sy-subrc EQ 0.
      flag_top = abap_true.
    ELSE.
      SELECT * FROM agr_users
        INTO TABLE lt_agr
       WHERE agr_name = c_support
         AND uname = sy-uname
         AND from_dat <= sy-datum
         AND to_dat >= sy-datum.
      IF sy-subrc EQ 0.
        flag_top = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ZHR_KPI_APP_DPC_EXT=>CHECK_TOP_MAN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_UNAME                       TYPE        UNAME
* | [--->] IM_DATE                        TYPE        ZHRE_UI_REQ_DATE(optional)
* | [<-()] FLAG_TOP                       TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD check_top_man.
    DATA: l_pernr   TYPE persno,
          ls_lead   TYPE zhr_objbif_objec_leader_s,
          _bipernr  TYPE REF TO zcl_objbif_pernr,
          _biplans  TYPE REF TO zhr_objbif_plans,
          it_attrib TYPE TABLE OF pt1222.

    DATA(lv_date) = COND datum( WHEN im_date IS NOT INITIAL THEN im_date ELSE sy-datum ).

    CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
      EXPORTING
        user_name               = im_uname
      IMPORTING
        employeenumber          = l_pernr
      EXCEPTIONS
        no_employeenumber_found = 1
        subtype_not_available   = 2
        OTHERS                  = 3.
    CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
      EXPORTING
        otype  = c_otype_p
        objid  = l_pernr
*       datum  = sy-datum   " del NaumovSM 02.02.2021
        datum  = lv_date   " ins NaumovSM 02.02.2021
      CHANGING
        result = _bipernr
      EXCEPTIONS
        OTHERS = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    TRY.
        _bipernr->zhr_objbif_pernr~get_orgass(
        EXPORTING
*          adatum = sy-datum    " del NaumovSM 02.02.2021
          adatum = lv_date   " ins NaumovSM 02.02.2021
          IMPORTING
            resplans = _biplans
        ).
        CLEAR: it_attrib[].
        CHECK _biplans IS NOT INITIAL.
        CALL FUNCTION 'RH_AUTHORITY_CHECK_OFF'.
        CALL FUNCTION 'RH_OM_ATTRIBUTES_READ'
          EXPORTING
            plvar            = '01'
            otype            = 'S'
            objid            = _biplans->objec-objid
*           seldate          = sy-datum    " del NaumovSM 02.02.2021
            seldate          = lv_date   " ins NaumovSM 02.02.2021
          TABLES
            attrib           = it_attrib
          EXCEPTIONS
            no_active_plvar  = 1
            no_attributes    = 2
            no_values        = 3
            object_not_found = 4
            OTHERS           = 5.
        READ TABLE it_attrib ASSIGNING FIELD-SYMBOL(<attr>) WITH KEY attrib = 'ZTOPMGR'.
        CHECK sy-subrc EQ 0.
        IF <attr>-low IS NOT INITIAL.
          flag_top = abap_true.
        ENDIF.
      CATCH zcx_objbif_objec.
    ENDTRY.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->DELEGSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_DELEG
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD delegset_get_entityset.
    DATA: _bipernr TYPE REF TO zhr_objbif_pernr,
          l_pernr  TYPE persno,
          lt_deleg TYPE TABLE OF hrus_d2.
    CONSTANTS: c_rep TYPE hrus_d2-reppr VALUE 'ZPREM'.
    SELECT * FROM hrus_d2
            INTO TABLE lt_deleg
            WHERE rep_name = sy-uname
              AND begda <= sy-datum
              AND endda >= sy-datum
              AND reppr = c_rep
              AND active = abap_true
             .
    LOOP AT lt_deleg ASSIGNING FIELD-SYMBOL(<deleg>).
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<entity>).
      <entity>-uname = <deleg>-us_name.
      CLEAR l_pernr.
      CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
        EXPORTING
          user_name               = CONV uname( <entity>-uname )
        IMPORTING
          employeenumber          = l_pernr
        EXCEPTIONS
          no_employeenumber_found = 1
          subtype_not_available   = 2
          OTHERS                  = 3.
      CLEAR _bipernr.
      m_get_objec c_otype_p l_pernr _bipernr sy-datum.
      IF _bipernr IS NOT INITIAL.
        <entity>-fio = _bipernr->get_name( adatum = sy-datum anoauth = abap_true ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->FILESET_GET_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IO_REQUEST_OBJECT              TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [<---] ER_ENTITY                      TYPE        ZCL_ZHR_KPI_APP_MPC=>TS_FILE
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD fileset_get_entity.
    DATA(lt_key) = io_tech_request_context->get_keys( ).
    READ TABLE lt_key INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_key>).

*    DATA(lt_keys) = io_tech_request_context->get_source_keys( ).
    TRY.
        DATA(l_reqid) = lt_key[ name = 'REQID' ]-value.
      CATCH cx_root.

    ENDTRY.
*        DATA(l_reqid) = lt_key[ name = 'REQID' ]-value.
        er_entity-reqid = <ls_key>-value.

*      ORDER BY ts DESCENDING.
   ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ZHR_KPI_APP_DPC_EXT=>GEN_URL
* +-------------------------------------------------------------------------------------------------+
* | [<-()] URL                            TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD gen_url.
    DATA: "url         TYPE string,
      lv_param       TYPE string,
      lv_protocol    TYPE string,
      lv_host        TYPE string,
      lv_port        TYPE string,
      lv_application TYPE string.
    cl_http_server=>if_http_server~get_location(
      EXPORTING
        application  = /ui2/if_start_url=>co_flp
      IMPORTING
        host         = lv_host
        port         = lv_port
        out_protocol = lv_protocol ).
*    IF lv_host IS INITIAL OR lv_port IS INITIAL OR lv_protocol IS INITIAL.
*      MESSAGE TEXT-100 TYPE 'E'.
*      RETURN.
*    ENDIF.

    CONCATENATE lv_protocol '://' lv_host ':' lv_port /ui2/if_start_url=>co_flp INTO url.
    TRANSLATE url TO LOWER CASE.

*  DATA lv_param       TYPE string.
    DATA lv_langu       TYPE c LENGTH 2.
    DATA lv_accessibility_mode TYPE c.

    " client
    CONCATENATE 'sap-client=' sy-mandt INTO lv_param.       "#EC NOTEXT
    IF url CA '?'.
      CONCATENATE url '&' lv_param INTO url.                "#EC NOTEXT
    ELSE.
      CONCATENATE url '?' lv_param INTO url.                "#EC NOTEXT
    ENDIF.
    CLEAR lv_param.

    " language
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = sy-langu
      IMPORTING
        output = lv_langu.
    CONCATENATE 'sap-language=' lv_langu INTO lv_param.     "#EC NOTEXT
    IF url CA '?'.
      CONCATENATE url '&' lv_param INTO url.                "#EC NOTEXT
    ELSE.
      CONCATENATE url '?' lv_param INTO url.                "#EC NOTEXT
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ZHR_KPI_APP_DPC_EXT=>GET_ACTUAL_ROUTE_LINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_ROUTE                       TYPE        ZHR_UI_APPROVER_T
* | [--->] IS_NEW_HEAD_REQUEST            TYPE        ZHRT_UI_KPI_GRA
* | [--->] IS_OLD_HEAD_REQUEST            TYPE        ZHRT_UI_KPI_GRA
* | [--->] IV_USER                        TYPE        SY-UNAME
* | [--->] IT_REQUEST                     TYPE        ZHR_UI_KPI_GRA_TT
* | [--->] IV_WAY                         TYPE        CHAR1
* | [<-()] ES_ROUTE                       TYPE        ZHR_UI_APPROVER_S
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_actual_route_line.

    DATA lv_req_idx    TYPE  zhr_ui_approver_s-idx.

    DATA(lt_route)            = it_route.
    DATA(lt_request)          = it_request.
    DATA(ls_old_head_request) = is_old_head_request.
    DATA(ls_new_head_request) = is_new_head_request.
    DATA(lv_user)             = iv_user.
    DATA(lv_way)              = iv_way.

    LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_request_init>) WHERE status = '01'.
      IF <ls_request_init>-uname = <ls_request_init>-aenam AND
         <ls_request_init>-approver = <ls_request_init>-aenam.
        DELETE lt_request INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

***** { ins NaumovSM
***** если актуальная запись отклонена ищем предыдущую неотклонённую запись по данному маршруту
****    IF is_new_head_request-declinecommentary IS NOT INITIAL.
****      DO lines( lt_request ) TIMES.
****        DATA(lv_actual_idx) = lines( lt_request ) - sy-index + 1.
***** считываем предыдущую не отклонённую запись с предыдущим индексом маршрута
****        LOOP AT lt_request FROM lv_actual_idx ASSIGNING FIELD-SYMBOL(<ls_actual_req>) WHERE declinecommentary IS INITIAL.
*****                                                                AND idx = ( <req>-idx - 1 ).
****          EXIT.
****        ENDLOOP.
****        IF sy-subrc = 0.
****          DATA(lv_actual_approver) = <ls_actual_req>-aenam.
****          EXIT.
****        ENDIF.
****      ENDDO.
****    ELSE.
****      READ TABLE lt_request ASSIGNING FIELD-SYMBOL(<ls_last_request>) INDEX lines( lt_request ).
****    ENDIF.
***** } ins NaumovSM

    LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_last_request>) FROM lines( lt_request ).
      .
      IF ( <ls_last_request>-aenam = <ls_last_request>-approver OR
           <ls_last_request>-deleg = <ls_last_request>-approver ) OR
         <ls_last_request>-declinecommentary   <> '' OR
         is_new_head_request-declinecommentary <> ''.   " отозвано на доработку / отклонено

        IF is_new_head_request-declinecommentary = ''.  " обработка текущего отклонения
          IF <ls_last_request>-idx <> 1.
            IF <ls_last_request>-approver = <ls_last_request>-aenam.
              lv_req_idx = <ls_last_request>-idx.
            ELSE.
              lv_req_idx = <ls_last_request>-idx - 1.
            ENDIF.
          ELSE.
            IF <ls_last_request>-approver = <ls_last_request>-aenam.
              lv_req_idx = <ls_last_request>-idx - 1.
            ELSE.
              lv_req_idx = <ls_last_request>-idx.
            ENDIF.
          ENDIF.
        ELSE.
          IF <ls_last_request>-approver = <ls_last_request>-aenam.
            lv_req_idx = <ls_last_request>-idx - 1.   " обработка отклонения из прошлой записи
          ELSE.
            lv_req_idx = <ls_last_request>-idx.
          ENDIF.
        ENDIF.

        CASE lv_way.
          WHEN 'B'.
            lv_req_idx = lv_req_idx.
          WHEN 'F'.
            IF <ls_last_request>-approver <> <ls_last_request>-aenam AND <ls_last_request>-status <> c_st_04.
              lv_req_idx = lv_req_idx + 1.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.

        READ TABLE lt_route INDEX lv_req_idx ASSIGNING FIELD-SYMBOL(<ls_route>).   " считываем актуальную
        IF sy-subrc <> 0.
*        READ TABLE lt_route INDEX ( lv_req_idx - 1 ) ASSIGNING FIELD-SYMBOL(<ls_route>).   " последняя запись в маршруте
*        IF sy-subrc <> 0.
          UNASSIGN <ls_route>.
*          ENDIF.
        ELSE.
*          CASE lv_way.
*            WHEN 'B'.
*              lv_req_idx = lv_req_idx.
*            WHEN 'F'.
*              IF <ls_last_request>-approver <> <ls_last_request>-aenam.
*                lv_req_idx = lv_req_idx + 1.
*              ENDIF.
*            WHEN OTHERS.
*          ENDCASE.
        ENDIF.
      ELSE.
        CASE lv_way.
          WHEN 'B'.
            lv_req_idx = <ls_last_request>-idx - 1.
          WHEN 'F'.
            IF <ls_last_request>-approver <> <ls_last_request>-aenam.
              lv_req_idx = <ls_last_request>-idx + 1.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
      EXIT.
    ENDLOOP.
    IF lv_req_idx IS INITIAL.    " первая запись
      CASE lv_way.
        WHEN 'B'.
          lv_req_idx = 1.
        WHEN 'F'.
          IF lt_request IS INITIAL.
            lv_req_idx = 1.
          ELSE.
            lv_req_idx = 2.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ELSE.
      READ TABLE lt_route INDEX lv_req_idx ASSIGNING <ls_route>.
    ENDIF.
*    IF lv_req_idx IS INITIAL.    " первая запись
*      lv_req_idx = 1.
*    ENDIF.

*    IF <ls_route> IS NOT ASSIGNED.
* доп проверки
    LOOP AT lt_route ASSIGNING FIELD-SYMBOL(<ls_main_route>) WHERE uname = lv_user
                                                               AND idx   = lv_req_idx.
      DATA(lv_route_idx) = <ls_main_route>-idx.
      DATA(lv_prev_idx) = <ls_main_route>-idx - 1.
      DATA(lv_next_idx) = <ls_main_route>-idx + 1.
*      CHECK lv_prev_idx > 0.
      READ TABLE lt_route INDEX lv_prev_idx ASSIGNING FIELD-SYMBOL(<ls_prev_route>).
      IF sy-subrc = 0.
* Проверяем что запись не была позднее отклонена
        DATA(lv_next_request_idx) = <ls_prev_route>-idx + 1.
        READ TABLE lt_request INDEX ( lines( lt_request ) - 1 ) ASSIGNING FIELD-SYMBOL(<ls_request>).
        IF sy-subrc = 0.
          IF <ls_request>-declinecommentary = ''.
            IF ls_old_head_request-aenam = <ls_request>-approver OR
               ls_old_head_request-deleg = <ls_request>-approver.

              READ TABLE lt_route INDEX lv_next_idx ASSIGNING <ls_route>.
              IF sy-subrc <> 0.
                READ TABLE lt_route INDEX lv_route_idx ASSIGNING <ls_route>.   " последняя запись в маршруте
                IF sy-subrc <> 0.
                  UNASSIGN <ls_route>.
                ENDIF.
              ENDIF.
              EXIT.
            ELSE.

              READ TABLE lt_route INDEX lv_req_idx ASSIGNING <ls_route>.
              IF sy-subrc <> 0.
                UNASSIGN <ls_route>.
              ENDIF.
              EXIT.
            ENDIF.
          ELSE.

            READ TABLE lt_route INDEX lv_next_idx ASSIGNING <ls_route>.
            IF sy-subrc <> 0.
              UNASSIGN <ls_route>.
            ENDIF.
            EXIT.
          ENDIF.
        ELSE.
          IF <ls_prev_route>-uname = ls_old_head_request-aenam OR
             <ls_prev_route>-uname = ls_old_head_request-deleg.

            READ TABLE lt_route INDEX lv_next_idx ASSIGNING <ls_route>.    " считываем следующую актуальную
            IF sy-subrc <> 0.
              READ TABLE lt_route INDEX lv_route_idx ASSIGNING <ls_route>.   " последняя запись в маршруте
              IF sy-subrc <> 0.
                UNASSIGN <ls_route>.
              ENDIF.
            ENDIF.
*            IF sy-subrc = 0.
*              lv_next_uname = <ls_next_route>-uname.
*            ENDIF.

            EXIT.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE lt_route INDEX lv_next_idx ASSIGNING <ls_route>.
        IF sy-subrc <> 0.
          UNASSIGN <ls_route>.
        ENDIF.
      ENDIF.
    ENDLOOP.

*    ENDIF.

    IF <ls_route> IS ASSIGNED.
      es_route = <ls_route>.
    ENDIF.

*
*    LOOP AT lt_route ASSIGNING FIELD-SYMBOL(<ls_route>) WHERE uname = lv_user.
*      DATA(lv_route_idx) = sy-tabix.
*      DATA(lv_prev_idx) = sy-tabix - 1.
*      DATA(lv_next_idx) = sy-tabix + 1.
*      CHECK lv_prev_idx > 0.
*      READ TABLE lt_route INDEX lv_prev_idx ASSIGNING FIELD-SYMBOL(<ls_prev_route>).
*      IF sy-subrc = 0.
** Проверяем что запись не была позднее отклонена
*        DATA(lv_next_request_idx) = <ls_route>-idx + 1.
*        READ TABLE lt_request WITH KEY idx = lv_next_request_idx ASSIGNING FIELD-SYMBOL(<ls_request>).
*        IF sy-subrc = 0.
*          IF <ls_request>-declinecommentary IS NOT INITIAL.
*            IF ls_new_head_request-aenam = <ls_request>-approver OR
*               ls_new_head_request-deleg = <ls_request>-approver.
*
*              READ TABLE lt_route INDEX lv_next_idx ASSIGNING FIELD-SYMBOL(<ls_next_route>).
**              IF sy-subrc = 0.
**                DATA(lv_next_uname) = <ls_next_route>-uname.
**              ENDIF.
*
*              EXIT.
*            ENDIF.
*          ENDIF.
*        ELSE.
*          IF <ls_prev_route>-uname = ls_old_head_request-aenam OR
*             <ls_prev_route>-uname = ls_old_head_request-deleg.
*
*            READ TABLE lt_route INDEX lv_next_idx ASSIGNING <ls_next_route>.
**            IF sy-subrc = 0.
**              lv_next_uname = <ls_next_route>-uname.
**            ENDIF.
*
*            EXIT.
*          ELSE.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    IF <ls_next_route> IS ASSIGNED.
*      es_route = <ls_next_route>.
*    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ZHR_KPI_APP_DPC_EXT=>GET_APPR_ROUTE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_AUTHOR                      TYPE        UNAME
* | [--->] IV_NOSEND                      TYPE        FLAG(optional)
* | [--->] IV_YEAR                        TYPE        JAHR(optional)
* | [<-->] CH_TAB                         TYPE        ZHR_UI_APPROVER_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_appr_route.
    DATA: ls_lead   TYPE zhr_objbif_objec_leader_s,
          _bipernr  TYPE REF TO zcl_objbif_pernr,
          _biorgeh  TYPE REF TO zhr_objbif_orgeh,
          lt_struc  TYPE TABLE OF struc,
          lt_p0105  TYPE TABLE OF p0105,
          lt_p0001  TYPE TABLE OF p0001,
          l_objid   TYPE objektid,
          l_pernr   TYPE persno,
          ret_uname TYPE uname,
          r_appr    TYPE RANGE OF uname,
          r_orgeh   TYPE RANGE OF orgeh,
          l_index   TYPE i,
          l_year    TYPE jahr,
          lv_zam    TYPE persno.

    DATA: lt_hire_fire_pernr TYPE TABLE OF phifi.   " ins NaumovSM

    IF iv_year IS INITIAL.
      l_year = sy-datum+0(4).
    ELSE.
      l_year = iv_year.
    ENDIF.

    CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
      EXPORTING
        user_name               = im_author
      IMPORTING
        employeenumber          = l_pernr
      EXCEPTIONS
        no_employeenumber_found = 1
        subtype_not_available   = 2
        OTHERS                  = 3.

* { ins NaumovSM
    IF sy-datum+0(4) = iv_year.
      DATA(lv_date) = sy-datum.
    ELSE.
      lv_date = |{ l_year }1231|.
    ENDIF.
* } ins NaumovSM
* { ins NaumovSM Добавляем анализ на уволенность сотрудника (для просмотра в будущем)
    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HRF_HIRE_FIRE'
      EXPORTING
        pernr         = l_pernr
*       BEGDA         = '18000101'
*       ENDDA         = '99991231'
*       LANGUAGE      =
*       PERSON        =
      TABLES
        ms_data       = lt_hire_fire_pernr
      EXCEPTIONS
        reject_pernr  = 1
        display_error = 2
        OTHERS        = 3.
    IF sy-subrc = 0.
      SORT lt_hire_fire_pernr DESCENDING BY endda.
      LOOP AT lt_hire_fire_pernr ASSIGNING FIELD-SYMBOL(<ls_hire_fire>)
                                     WHERE stat2 = '3'.
        IF lv_date > <ls_hire_fire>-endda.
          lv_date = <ls_hire_fire>-endda.
        ENDIF.
        EXIT.
      ENDLOOP.
* Implement suitable error handling here
    ENDIF.
* } ins NaumovSM


    REFRESH ch_tab.
    CLEAR ch_tab[].
*    m_read_inf l_pernr '0001' lt_p0001.   " del NaumovSM
    m_read_inf l_pernr '0001' lt_p0001 lv_date.   " ins NaumovSM
    l_objid = VALUE #( lt_p0001[ 1 ]-orgeh OPTIONAL ).
    APPEND INITIAL LINE TO ch_tab ASSIGNING FIELD-SYMBOL(<line>).
    <line>-uname = im_author.
    <line>-pernr = l_pernr.
    CHECK l_objid IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '00000000' ) TO r_orgeh.
***Собираем маршрут без доп согласующих************************************************
    CLEAR: lt_struc[].
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype       = c_otype_o
        act_objid       = l_objid
        act_wegid       = c_wegid_up
*       act_begda       = sy-datum   " del NaumovSM
*       act_endda       = sy-datum   " del NaumovSM
        act_begda       = lv_date   " ins NaumovSM
        act_endda       = lv_date   " ins NaumovSM
        authority_check = ''
      TABLES
        result_struc    = lt_struc
      EXCEPTIONS
        no_plvar_found  = 1
        no_entry_found  = 2
        OTHERS          = 3.
    LOOP AT lt_struc ASSIGNING FIELD-SYMBOL(<struc>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <struc>-objid ) TO r_orgeh.
      CLEAR: _biorgeh, ls_lead.
*      m_get_objec c_otype_o <struc>-objid _biorgeh.   " del NaumovSM
      m_get_objec c_otype_o <struc>-objid _biorgeh lv_date.   " ins NaumovSM
      CHECK _biorgeh IS NOT INITIAL.
*!!!!
*      _biorgeh->get_leader(
*       EXPORTING
**         adatum = sy-datum   " del NaumovSM
*         adatum = lv_date   " ins NaumovSM
*       IMPORTING
*         result = ls_lead
*              ).
      zcl_zhr_fi_tv_req_dpc_ext=>get_leader(
                                   EXPORTING
                                     iv_objid = CONV objid( <struc>-objid )
                                     iv_date = sy-datum
                                   IMPORTING
                                     es_lead = ls_lead
                                          ).
*      CHECK ls_lead-pernr IS NOT INITIAL.
      CLEAR lt_p0105[].
      CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          pernr           = ls_lead-pernr
          infty           = '0105'
*         begda           = sy-datum   " del NaumovSM
*         endda           = sy-datum   " del NaumovSM
          begda           = lv_date   " ins NaumovSM
          endda           = lv_date   " ins NaumovSM
        TABLES
          infty_tab       = lt_p0105
        EXCEPTIONS
          infty_not_found = 1
          invalid_input   = 2
          OTHERS          = 3.
      ret_uname = VALUE #( lt_p0105[ subty = '0001' ]-usrid OPTIONAL ).
*{ 09.06.2021 mvprokofyev 7700157035 Ищем заместителя
      CLEAR lv_zam.
      CHECK ls_lead-plans IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(ls_hrp1007)
          FROM hrp1007
          WHERE objid = @ls_lead-plans
            AND status = '0'
            AND plvar = '01'
            AND otype = 'S'
            AND begda <= @sy-datum
            AND endda >= @sy-datum.
      IF sy-subrc IS INITIAL AND ls_hrp1007-status = '0'.
* READ_INFOTYPE не подходит, так как выборка идет не потабельному
        SELECT SINGLE pernr
          FROM pa2003
          INTO lv_zam
            WHERE subty = '02'
              AND endda >= sy-datum
              AND begda <= sy-datum
              AND plans = ls_lead-plans.
        IF lv_zam IS NOT INITIAL.
          CLEAR lt_p0105[].
          CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
          m_read_inf lv_zam '0105' lt_p0105 sy-datum.
          ret_uname = VALUE #( lt_p0105[ subty = '0001' ]-usrid OPTIONAL ).
         ENDIF.
      ENDIF.
* 09.06.2021 mvprokofyev 7700157035}

      CHECK ret_uname IS NOT INITIAL AND ls_lead-pernr NE l_pernr.
      READ TABLE lt_p0105 WITH KEY subty = c_ses INTO DATA(ls_p0105).
      IF sy-subrc = 0.    " ins NaumovSM
        DATA(l_flag) = ls_p0105-zzflag.
      ELSE.
        CLEAR: l_flag.
      ENDIF.    " ins NaumovSM
      IF l_flag IS INITIAL AND iv_nosend IS INITIAL.
*        send_noses( imper = ls_lead-pernr ).
      ENDIF.
      APPEND INITIAL LINE TO ch_tab ASSIGNING <line>.
      <line>-uname = ret_uname.
      <line>-orgeh = <struc>-objid.
      <line>-pernr = ls_lead-pernr.
*{ 09.06.2021 mvprokofyev 7700157035
      IF lv_zam IS NOT INITIAL.
        <line>-zam = abap_true.
      ENDIF.
* 09.06.2021 mvprokofyev 7700157035}
* { ins NaumovSM
      IF l_flag IS INITIAL.
        <line>-no_ses = abap_true.
      ENDIF.
* } ins NaumovSM
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ret_uname ) TO r_appr.
*      IF check_top_man( im_uname = ret_uname ) = abap_true.   " del NaumovSM
      IF check_top_man( im_uname = ret_uname
                        im_date  = CONV #( lv_date ) ) = abap_true.   " ins NaumovSM
        <line>-top = abap_true.
        EXIT.
      ENDIF.
      CLEAR: ls_p0105.     " ins NaumovSM.
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM ch_tab COMPARING uname.
*****************************************************************************
*   Считываем доп.согласующих
    DATA lt_appr TYPE TABLE OF zhrt_ui_approver.
    SELECT * FROM zhrt_ui_approver INTO TABLE lt_appr WHERE ( approver IN r_appr OR add_approver IN r_appr ) AND orgeh IN r_orgeh AND year_appr = l_year.
*   Добавляем согласующих "после меня"
    LOOP AT ch_tab INTO DATA(ls_tab) WHERE add_fl = abap_false.
      CHECK sy-tabix > 1.
      l_index = sy-tabix + 1.
      READ TABLE lt_appr WITH KEY approver = ls_tab-uname queue = c_queue_after ASSIGNING FIELD-SYMBOL(<fs_appr>).
      IF sy-subrc EQ 0.
* <-- ins NaumovSM 14.12.2020
        CLEAR l_pernr.
        CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
          EXPORTING
            user_name               = <fs_appr>-add_approver
          IMPORTING
            employeenumber          = l_pernr
          EXCEPTIONS
            no_employeenumber_found = 1
            subtype_not_available   = 2
            OTHERS                  = 3.
        INSERT VALUE #( uname = <fs_appr>-add_approver pernr = l_pernr add_fl = abap_true ) INTO ch_tab INDEX l_index.
* --> ins NaumovSM 14.12.2020
*        INSERT VALUE #( uname = <fs_appr>-add_approver add_fl = abap_true ) INTO ch_tab INDEX l_index.   " del NaumovSM 14.12.2020
      ENDIF.
    ENDLOOP.
*   Добавляем согласующих "Передо мной"
    LOOP AT ch_tab INTO ls_tab WHERE add_fl = abap_false.
      CHECK sy-tabix > 1.
      l_index = sy-tabix.
      READ TABLE lt_appr WITH KEY approver = ls_tab-uname queue = c_queue_before ASSIGNING <fs_appr>.
      IF sy-subrc EQ 0.
* <-- ins NaumovSM 14.12.2020
        CLEAR l_pernr.
        CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
          EXPORTING
            user_name               = <fs_appr>-add_approver
          IMPORTING
            employeenumber          = l_pernr
          EXCEPTIONS
            no_employeenumber_found = 1
            subtype_not_available   = 2
            OTHERS                  = 3.
        INSERT VALUE #( uname = <fs_appr>-add_approver pernr = l_pernr add_fl = abap_true ) INTO ch_tab INDEX l_index.
* --> ins NaumovSM 14.12.2020
*        INSERT VALUE #( uname = <fs_appr>-add_approver add_fl = abap_true ) INTO ch_tab INDEX l_index.   " del NaumovSM 14.12.2020
      ENDIF.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM ch_tab COMPARING uname. " <-- ins NaumovSM

    LOOP AT ch_tab ASSIGNING FIELD-SYMBOL(<ls_tab>).
      <ls_tab>-idx = sy-tabix.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ZHR_KPI_APP_DPC_EXT=>GET_APPR_ROUTE_ALL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_AUTHOR                      TYPE        UNAME
* | [--->] IV_NOSEND                      TYPE        FLAG(optional)
* | [--->] IV_YEAR                        TYPE        JAHR(optional)
* | [<-->] CH_TAB                         TYPE        ZHR_UI_APPROVER_T_ALL
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_appr_route_all.
    DATA: ls_lead    TYPE zhr_objbif_objec_leader_s,
          _bipernr   TYPE REF TO zcl_objbif_pernr,
          _biorgeh   TYPE REF TO zhr_objbif_orgeh,
          lt_struc   TYPE TABLE OF struc,
          lt_p0105   TYPE TABLE OF p0105,
          lt_p0001   TYPE TABLE OF p0001,
          l_objid    TYPE objektid,
          l_pernr    TYPE persno,
          ret_uname  TYPE uname,
          r_appr     TYPE RANGE OF uname,
          r_orgeh    TYPE RANGE OF orgeh,
          l_index    TYPE i,
          l_year     TYPE jahr,
          lv_zam     TYPE persno.

    DATA: lt_hire_fire_pernr TYPE TABLE OF phifi.   " ins NaumovSM

    IF iv_year IS INITIAL.
      l_year = sy-datum+0(4).
    ELSE.
      l_year = iv_year.
    ENDIF.

    CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
      EXPORTING
        user_name               = im_author
      IMPORTING
        employeenumber          = l_pernr
      EXCEPTIONS
        no_employeenumber_found = 1
        subtype_not_available   = 2
        OTHERS                  = 3.

* { ins NaumovSM
    IF sy-datum+0(4) = iv_year.
      DATA(lv_date) = sy-datum.
    ELSE.
      lv_date = |{ l_year }1231|.
    ENDIF.
* } ins NaumovSM
* { ins NaumovSM Добавляем анализ на уволенность сотрудника (для просмотра в будущем)
    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HRF_HIRE_FIRE'
      EXPORTING
        pernr         = l_pernr
*       BEGDA         = '18000101'
*       ENDDA         = '99991231'
*       LANGUAGE      =
*       PERSON        =
      TABLES
        ms_data       = lt_hire_fire_pernr
      EXCEPTIONS
        reject_pernr  = 1
        display_error = 2
        OTHERS        = 3.
    IF sy-subrc = 0.
      SORT lt_hire_fire_pernr DESCENDING BY endda.
      LOOP AT lt_hire_fire_pernr ASSIGNING FIELD-SYMBOL(<ls_hire_fire>)
                                     WHERE stat2 = '3'.
        IF lv_date > <ls_hire_fire>-endda.
          lv_date = <ls_hire_fire>-endda.
        ENDIF.
        EXIT.
      ENDLOOP.
* Implement suitable error handling here
    ENDIF.
* } ins NaumovSM


    REFRESH ch_tab.
    CLEAR ch_tab[].
*    m_read_inf l_pernr '0001' lt_p0001.   " del NaumovSM
    m_read_inf l_pernr '0001' lt_p0001 lv_date.   " ins NaumovSM
    l_objid = VALUE #( lt_p0001[ 1 ]-orgeh OPTIONAL ).
    APPEND INITIAL LINE TO ch_tab ASSIGNING FIELD-SYMBOL(<line>).
    <line>-uname = im_author.
    <line>-pernr = l_pernr.
    CHECK l_objid IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '00000000' ) TO r_orgeh.
***Собираем маршрут без доп согласующих************************************************
    CLEAR: lt_struc[].
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype       = c_otype_o
        act_objid       = l_objid
        act_wegid       = c_wegid_up
*       act_begda       = sy-datum   " del NaumovSM
*       act_endda       = sy-datum   " del NaumovSM
        act_begda       = lv_date   " ins NaumovSM
        act_endda       = lv_date   " ins NaumovSM
        authority_check = ''
      TABLES
        result_struc    = lt_struc
      EXCEPTIONS
        no_plvar_found  = 1
        no_entry_found  = 2
        OTHERS          = 3.
    LOOP AT lt_struc ASSIGNING FIELD-SYMBOL(<struc>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <struc>-objid ) TO r_orgeh.
      CLEAR: _biorgeh, ls_lead.
*      m_get_objec c_otype_o <struc>-objid _biorgeh.   " del NaumovSM
      m_get_objec c_otype_o <struc>-objid _biorgeh sy-datum.
      CHECK _biorgeh IS NOT INITIAL.
      zcl_zhr_fi_tv_req_dpc_ext=>get_leader(
                                   EXPORTING
                                     iv_objid = CONV objid( <struc>-objid )
                                     iv_date = sy-datum
                                   IMPORTING
                                     es_lead = ls_lead
                                          ).
*      CHECK ls_lead-pernr IS NOT INITIAL.
      CLEAR lt_p0105[].
      CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          pernr           = ls_lead-pernr
          infty           = '0105'
*         begda           = sy-datum   " del NaumovSM
*         endda           = sy-datum   " del NaumovSM
          begda           = lv_date   " ins NaumovSM
          endda           = lv_date   " ins NaumovSM
        TABLES
          infty_tab       = lt_p0105
        EXCEPTIONS
          infty_not_found = 1
          invalid_input   = 2
          OTHERS          = 3.
      ret_uname = VALUE #( lt_p0105[ subty = '0001' ]-usrid OPTIONAL ).
*{ 09.06.2021 mvprokofyev 7700157035 Ищем заместителя
      CLEAR lv_zam.
      CHECK ls_lead-plans IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(ls_hrp1007)
          FROM hrp1007
          WHERE objid = @ls_lead-plans
            AND status = '0'
            AND plvar = '01'
            AND otype = 'S'
            AND begda <= @sy-datum
            AND endda >= @sy-datum.
      IF sy-subrc IS INITIAL AND ls_hrp1007-status = '0'.
* READ_INFOTYPE не подходит, так как выборка идет не потабельному
        SELECT SINGLE pernr
          FROM pa2003
          INTO lv_zam
            WHERE subty = '02'
              AND endda >= sy-datum
              AND begda <= sy-datum
              AND plans = ls_lead-plans.
        IF lv_zam IS NOT INITIAL.
          CLEAR lt_p0105[].
          CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
          m_read_inf lv_zam '0105' lt_p0105 sy-datum.
          ret_uname = VALUE #( lt_p0105[ subty = '0001' ]-usrid OPTIONAL ).
         ENDIF.
      ENDIF.
* 09.06.2021 mvprokofyev 7700157035}
*      CHECK ret_uname IS NOT INITIAL AND ls_lead-pernr NE l_pernr.
      READ TABLE lt_p0105 WITH KEY subty = c_ses INTO DATA(ls_p0105).
      IF sy-subrc = 0.    " ins NaumovSM
        DATA(l_flag) = ls_p0105-zzflag.
      ELSE.
        CLEAR: l_flag.
      ENDIF.    " ins NaumovSM
      IF l_flag IS INITIAL AND iv_nosend IS INITIAL.
*        send_noses( imper = ls_lead-pernr ).
      ENDIF.
      APPEND INITIAL LINE TO ch_tab ASSIGNING <line>.
      <line>-uname = ret_uname.
      <line>-orgeh = <struc>-objid.
      <line>-pernr = ls_lead-pernr.
      IF ls_lead-pernr IS INITIAL." AND ls_lead-plans IS NOT INITIAL.
        CHECK ls_lead-plans IS NOT INITIAL.
*проверяем, является ли должность вакантной
        CHECK ls_hrp1007 IS  NOT INITIAL.
        IF ls_hrp1007-status = '0' AND lv_zam IS INITIAL.
          <line>-vacancy = abap_true.
        ENDIF.

      ENDIF.
      IF ret_uname IS INITIAL AND <line>-vacancy = abap_false.
        <line>-without_s = abap_true.
      ENDIF.
* { ins NaumovSM
      IF l_flag IS INITIAL.
        <line>-no_ses = abap_true.
      ENDIF.
* } ins NaumovSM
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ret_uname ) TO r_appr.
*      IF check_top_man( im_uname = ret_uname ) = abap_true.   " del NaumovSM
      IF check_top_man( im_uname = ret_uname
                        im_date  = CONV #( lv_date ) ) = abap_true.   " ins NaumovSM
        <line>-top = abap_true.
        EXIT.
      ENDIF.
      CLEAR: ls_p0105.     " ins NaumovSM.
    ENDLOOP.
*    DELETE ADJACENT DUPLICATES FROM ch_tab COMPARING uname.
*****************************************************************************
*   Считываем доп.согласующих
    DATA lt_appr TYPE TABLE OF zhrt_ui_approver.
    SELECT * FROM zhrt_ui_approver INTO TABLE lt_appr WHERE ( approver IN r_appr OR add_approver IN r_appr ) AND orgeh IN r_orgeh AND year_appr = l_year.
*   Добавляем согласующих "после меня"
    LOOP AT ch_tab INTO DATA(ls_tab) WHERE add_fl = abap_false.
      CHECK sy-tabix > 1.
      l_index = sy-tabix + 1.
      READ TABLE lt_appr WITH KEY approver = ls_tab-uname queue = c_queue_after ASSIGNING FIELD-SYMBOL(<fs_appr>).
      IF sy-subrc EQ 0.
* <-- ins NaumovSM 14.12.2020
        CLEAR l_pernr.
        CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
          EXPORTING
            user_name               = <fs_appr>-add_approver
          IMPORTING
            employeenumber          = l_pernr
          EXCEPTIONS
            no_employeenumber_found = 1
            subtype_not_available   = 2
            OTHERS                  = 3.
        INSERT VALUE #( uname = <fs_appr>-add_approver pernr = l_pernr add_fl = abap_true ) INTO ch_tab INDEX l_index.
* --> ins NaumovSM 14.12.2020
*        INSERT VALUE #( uname = <fs_appr>-add_approver add_fl = abap_true ) INTO ch_tab INDEX l_index.   " del NaumovSM 14.12.2020
      ENDIF.
    ENDLOOP.
*   Добавляем согласующих "Передо мной"
    LOOP AT ch_tab INTO ls_tab WHERE add_fl = abap_false.
      CHECK sy-tabix > 1.
      l_index = sy-tabix.
      READ TABLE lt_appr WITH KEY approver = ls_tab-uname queue = c_queue_before ASSIGNING <fs_appr>.
      IF sy-subrc EQ 0.
* <-- ins NaumovSM 14.12.2020
        CLEAR l_pernr.
        CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
          EXPORTING
            user_name               = <fs_appr>-add_approver
          IMPORTING
            employeenumber          = l_pernr
          EXCEPTIONS
            no_employeenumber_found = 1
            subtype_not_available   = 2
            OTHERS                  = 3.
        INSERT VALUE #( uname = <fs_appr>-add_approver pernr = l_pernr add_fl = abap_true ) INTO ch_tab INDEX l_index.
* --> ins NaumovSM 14.12.2020
*        INSERT VALUE #( uname = <fs_appr>-add_approver add_fl = abap_true ) INTO ch_tab INDEX l_index.   " del NaumovSM 14.12.2020
      ENDIF.
    ENDLOOP.

*    DELETE ADJACENT DUPLICATES FROM ch_tab COMPARING uname. " <-- ins NaumovSM
    LOOP AT ch_tab ASSIGNING <line>.
      LOOP AT ch_tab FROM sy-tabix + 1 ASSIGNING FIELD-SYMBOL(<line2>).
        IF <line2>-uname IS NOT INITIAL AND <line2>-uname EQ <line>-uname.
          <line>-nep_boss = COND #( WHEN <line2>-nep_boss IS NOT INITIAL THEN abap_true ELSE <line>-nep_boss ).
          <line>-ssp_boss = COND #( WHEN <line2>-ssp_boss IS NOT INITIAL THEN abap_true ELSE <line>-ssp_boss ).
          <line>-top = COND #( WHEN <line2>-top IS NOT INITIAL THEN abap_true ELSE <line>-top ).
          DELETE TABLE ch_tab FROM <line2>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT ch_tab ASSIGNING FIELD-SYMBOL(<ls_tab>).
      <ls_tab>-idx = sy-tabix.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->GET_DATA_KPI
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_REQID_STR                   TYPE        ZHRE_UI_REQID
* | [--->] IM_YEAR                        TYPE        GJAHR
* | [<-()] RV_DATA_KPI                    TYPE        ZCL_ZHR_KPI_APP_MPC=>TS_PERSON
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_data_kpi.
    DATA:_bipernr   TYPE REF TO zhr_objbif_pernr,
         _biplans   TYPE REF TO zhr_objbif_plans,
         _biorgeh   TYPE REF TO zhr_objbif_orgeh,
         lo_biplans TYPE REF TO zhr_objbif_plans,
         lo_biorgeh TYPE REF TO zhr_objbif_orgeh,
         l_pernr    TYPE persno,
         l_pernr_s  TYPE persno,   " ins NaumovSM
         l_uname    TYPE uname,
         l_textname TYPE thead-tdname,
         lt_text    TYPE zcl_zhr_kpi_app_mpc=>tt_goal,
         ls_lead    TYPE zhr_objbif_objec_leader_s,
         ls_lead_s  TYPE zhr_objbif_objec_leader_s,   " ins NaumovSM
         lt_p0105   TYPE TABLE OF p0105,
         lt_p0002   TYPE TABLE OF p0002,
         l_date     TYPE dats,
         l_text     TYPE string.   " ins NaumovSM
    DATA: lt_hire_fire_pernr TYPE TABLE OF phifi. " ins NaumovSM проверка на уволенного

    l_date = |{ im_year }1231|.
*    SELECT SINGLE grade1, grade2, procent, yeara, reqid
*      FROM zhrt_ui_kpi_gra
*      INTO @DATA(ls_gr)
*      WHERE uname = @im_reqid_str
*      AND yeara = @im_year.
    rv_data_kpi-year = im_year.
    SELECT SINGLE *"grade1, grade2, procent, yeara AS year, reqid, uname
          FROM zhrt_ui_kpi_gra
          INTO CORRESPONDING FIELDS OF rv_data_kpi
          WHERE reqid = im_reqid_str
          AND yeara = im_year.
    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
      EXPORTING
        user_name               = rv_data_kpi-uname
      IMPORTING
        employeenumber          = l_pernr
      EXCEPTIONS
        no_employeenumber_found = 1
        subtype_not_available   = 2
        OTHERS                  = 3.
*    rv_data_kpi-uname = l_uname.
    rv_data_kpi-id = l_pernr.

* { ins NaumovSM Добавляем анализ на уволенность сотрудника (для просмотра в будущем)
    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HRF_HIRE_FIRE'
      EXPORTING
        pernr         = l_pernr
*       BEGDA         = '18000101'
*       ENDDA         = '99991231'
*       LANGUAGE      =
*       PERSON        =
      TABLES
        ms_data       = lt_hire_fire_pernr
      EXCEPTIONS
        reject_pernr  = 1
        display_error = 2
        OTHERS        = 3.
    IF sy-subrc = 0.
      SORT lt_hire_fire_pernr DESCENDING BY endda.
      LOOP AT lt_hire_fire_pernr ASSIGNING FIELD-SYMBOL(<ls_hire_fire>)
                                     WHERE stat2 = '3'.
        l_date = <ls_hire_fire>-endda.
        EXIT.
      ENDLOOP.
* Implement suitable error handling here
    ENDIF.
* } ins NaumovSM

    CLEAR: _bipernr, _biplans, _biorgeh.
    CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
      EXPORTING
        otype  = c_otype_p
        objid  = l_pernr
        datum  = l_date
      CHANGING
        result = _bipernr
      EXCEPTIONS
        OTHERS = 1.
    IF _bipernr IS NOT INITIAL.
      rv_data_kpi-ename = _bipernr->get_name( adatum = l_date anoauth = abap_true ).
      TRY.
          CLEAR: _biplans, _biorgeh.
          _bipernr->get_orgass(
            EXPORTING
              adatum = l_date
            IMPORTING
              resplans = _biplans
              resorgeh = _biorgeh
        ).
*         Наименование должности
          IF _biplans IS NOT INITIAL.
*                  rv_data_kpi-stell = _biplans->objec-objid.
            rv_data_kpi-plnam = _biplans->get_name( adatum = l_date anoauth = abap_true ).
            TRANSLATE rv_data_kpi-plnam+0(1) TO UPPER CASE.
          ENDIF.
          IF _biorgeh IS NOT INITIAL.
            rv_data_kpi-orgna = _biorgeh->get_name( adatum = l_date anoauth = abap_true ).
            DATA: _orgpath  TYPE zhr_objbif_objec_orgpath_t,
                  it_attrib TYPE TABLE OF pt1222.
            _biorgeh->get_orgpath(
                       EXPORTING
                         aworigin = 'X'
*                             aminlevel = 3
                         adatum = l_date
                       IMPORTING
                         result = _orgpath[]
                       ).
*            SORT _orgpath BY pobid DESCENDING. " некорректная сортировка " del NaumovSM
            SORT _orgpath BY level DESCENDING. " ins NaumovSM

*{23.01.2020 Серёгин  Иван
*            CLEAR _biorgeh.
*            _biorgeh->get_leader(
*                 EXPORTING
*                   adatum      = l_date
*                 IMPORTING
*                   result = ls_lead
*                        ).
*            CLEAR: _bipernr.
*            m_get_objec c_otype_p ls_lead-pernr _bipernr.
*            m_read_inf ls_lead-pernr '0002' lt_p0002.
*            IF lt_p0002[] IS NOT INITIAL AND ls_lead-pernr <> l_pernr.
*              rv_data_kpi-supvdir = |{ lt_p0002[ 1 ]-nachn } { lt_p0002[ 1 ]-vorna+0(1) }. { lt_p0002[ 1 ]-midnm+0(1) }.|.
*            ENDIF.
*} 23.01.2020 Серёгин  Иван

            LOOP AT _orgpath ASSIGNING FIELD-SYMBOL(<path>).
              CLEAR _biorgeh.
              CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
                EXPORTING
                  otype  = c_otype_o
                  objid  = <path>-pobid
                  datum  = l_date
                CHANGING
                  result = _biorgeh
                EXCEPTIONS
                  OTHERS = 1.
              CLEAR: it_attrib[].
              CALL FUNCTION 'RH_OM_ATTRIBUTES_READ'
                EXPORTING
                  plvar            = c_plvar
                  otype            = c_otype_o
                  objid            = <path>-pobid
                  seldate          = l_date
                TABLES
                  attrib           = it_attrib
                EXCEPTIONS
                  no_active_plvar  = 1
                  no_attributes    = 2
                  no_values        = 3
                  object_not_found = 4
                  OTHERS           = 5.
              IF rv_data_kpi-dirname IS INITIAL.
                READ TABLE it_attrib ASSIGNING FIELD-SYMBOL(<attr>) WITH KEY attrib = 'ZLEVEL' low = 'УП'.
                IF sy-subrc EQ 0.
                  rv_data_kpi-dirname = _biorgeh->get_name( adatum = l_date anoauth = abap_true ).
                ENDIF.
              ENDIF.
* { ins NaumovSM
              IF rv_data_kpi-supvdir IS INITIAL.
                CLEAR ls_lead.
                _biorgeh->get_leader(
                                      EXPORTING
                                        adatum  = l_date
                                      IMPORTING
                                        result = ls_lead
                                             ).
                IF ls_lead-pernr <> l_pernr.
                  CLEAR lt_p0002.
*                  m_read_inf ls_lead-pernr '0002' lt_p0002.
                  m_read_inf ls_lead-pernr '0002' lt_p0002 l_date.
                  IF lt_p0002[] IS NOT INITIAL.
                    rv_data_kpi-supvdir = |{ lt_p0002[ 1 ]-nachn } { lt_p0002[ 1 ]-vorna+0(1) }. { lt_p0002[ 1 ]-midnm+0(1) }.|.   " ins NaumovSM
                  ENDIF.
                ENDIF.
              ENDIF.
* } ins NaumovSM
              IF rv_data_kpi-podrname IS INITIAL.
                READ TABLE it_attrib ASSIGNING <attr> WITH KEY attrib = 'ZLEVEL' low = 'ДП'.
                IF sy-subrc EQ 0.
                  rv_data_kpi-podrname = _biorgeh->get_name( adatum = l_date anoauth = abap_true ).
*{23.01.2020 Серёгин Иван

                  CLEAR ls_lead.
                  _biorgeh->get_leader(
                                        EXPORTING
                                          adatum  = l_date
                                        IMPORTING
                                          result = ls_lead
                                               ).
                  CLEAR: _bipernr.
*                  m_get_objec c_otype_p ls_lead-pernr _bipernr.   " del NaumovSM
                  m_get_objec c_otype_p ls_lead-pernr _bipernr l_date.   " ins NaumovSM
                  CLEAR lt_p0002.   " ins NaumovSM
*                  m_read_inf ls_lead-pernr '0002' lt_p0002.
                  m_read_inf ls_lead-pernr '0002' lt_p0002 l_date.
                  IF lt_p0002[] IS NOT INITIAL.
                    rv_data_kpi-rdname = |{ lt_p0002[ 1 ]-vorna+0(1) }. { lt_p0002[ 1 ]-midnm+0(1) }. { lt_p0002[ 1 ]-nachn }|.
                    IF rv_data_kpi-supvdir IS INITIAL.   " ins NaumovSM
                      rv_data_kpi-supvdir = |{ lt_p0002[ 1 ]-nachn } { lt_p0002[ 1 ]-vorna+0(1) }. { lt_p0002[ 1 ]-midnm+0(1) }.|.   " ins NaumovSM
                    ENDIF.   " ins NaumovSM
                  ENDIF.
                  IF _bipernr IS NOT INITIAL.
*                    rv_data_kpi-rdname = _bipernr->get_name( adatum = sy-datum anoauth = abap_true ).
                    CLEAR: lo_biplans, lo_biorgeh.
                    _bipernr->get_orgass(
                       EXPORTING
                          adatum = l_date
                       IMPORTING
                         resplans = lo_biplans
                         resorgeh = lo_biorgeh
                           ).
                    IF lo_biplans IS NOT INITIAL.
                      rv_data_kpi-rdplname = lo_biplans->get_name( adatum = l_date anoauth = abap_true ).
*                    TRANSLATE <entity>-plnam+0(1) TO UPPER CASE.
                    ENDIF.
                  ENDIF.
*}23.01.2020 Серёгин Иван
                ENDIF.
              ENDIF.
              IF rv_data_kpi-vpname IS INITIAL.
                CLEAR ls_lead.
                _biorgeh->get_leader(
                  EXPORTING
                    adatum = l_date
                  IMPORTING
                    result = ls_lead
                         ).
                IF ls_lead-pernr IS INITIAL.
*                  CALL METHOD get_oe
*                    EXPORTING
*                      im_plans  = _biorgeh-objec-objid
*                    receiving
*                      ret_orgeh = .

                ENDIF.
                IF ls_lead-pernr IS NOT INITIAL.
                  CLEAR: it_attrib[].
                  CALL FUNCTION 'RH_OM_ATTRIBUTES_READ'
                    EXPORTING
                      plvar            = c_plvar
                      otype            = c_otype_s
                      objid            = ls_lead-plans
                      seldate          = l_date
                    TABLES
                      attrib           = it_attrib
                    EXCEPTIONS
                      no_active_plvar  = 1
                      no_attributes    = 2
                      no_values        = 3
                      object_not_found = 4
                      OTHERS           = 5.
                  READ TABLE it_attrib ASSIGNING <attr> WITH KEY attrib = 'ZTOPMGR' low = 'X'.
                  IF sy-subrc EQ 0.
*                    m_read_inf ls_lead-pernr '0105' lt_p0105.
                    m_read_inf ls_lead-pernr '0105' lt_p0105 l_date.
*                    <appr>-a3p = ls_lead-pernr.
*                    <appr>-a3 = VALUE #( lt_p0105[ subty = '0001' ]-usrid OPTIONAL ).
*                    m_read_inf ls_lead-pernr '0002' lt_p0002.
                    m_read_inf ls_lead-pernr '0002' lt_p0002 l_date.
                    IF lt_p0002[] IS NOT INITIAL.
                      rv_data_kpi-vpname = |{ lt_p0002[ 1 ]-vorna+0(1) }. { lt_p0002[ 1 ]-midnm+0(1) }. { lt_p0002[ 1 ]-nachn }|.
*                      <entity>-approver3 = |{ lt_p0002[ 1 ]-nachn } { lt_p0002[ 1 ]-vorna+0(1) }. { lt_p0002[ 1 ]-midnm+0(1) }.|.
                    ENDIF.
                  ENDIF.
* { ins NaumovSM
                  CLEAR: _bipernr.
*                  m_get_objec c_otype_p ls_lead-pernr _bipernr.   " del NaumovSM
                  m_get_objec c_otype_p ls_lead-pernr _bipernr l_date.   " ins NaumovSM
                  IF _bipernr IS NOT INITIAL.
                    CLEAR: lo_biplans
                         , lo_biorgeh
                         .
                    _bipernr->get_orgass(
                       EXPORTING
                          adatum = l_date
                       IMPORTING
                         resplans = lo_biplans
                         resorgeh = lo_biorgeh
                           ).
                    IF lo_biplans IS NOT INITIAL.
                      rv_data_kpi-vpplname = lo_biplans->get_name( adatum = l_date anoauth = abap_true ).
                    ENDIF.
                  ENDIF.
* } ins NaumovSM
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        CATCH cx_root.
      ENDTRY.
    ENDIF.

* { ins NaumovSM комментарий к понижению/повышению
    CLEAR l_text.
    l_textname = |{ rv_data_kpi-reqid }P{ rv_data_kpi-rdate }{ rv_data_kpi-rtime }|.
    CALL METHOD read_text(
      IMPORTING
        text_id = l_textname
      CHANGING
        text    = l_text ).
    rv_data_kpi-percentcommentary = l_text.
* } ins NaumovSM


    SELECT * FROM zhrt_ui_kpi
      INTO CORRESPONDING FIELDS OF TABLE lt_text
      WHERE reqid = im_reqid_str.

    LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<ent>).
*      APPEND INITIAL LINE TO lt_text ASSIGNING FIELD-SYMBOL()

*      rv_data_kpi-togoals-text = <ent>-text.
      l_textname = |{ <ent>-reqid }{ rv_data_kpi-uname }{ <ent>-purposeid }|.
      CALL METHOD read_text(
        IMPORTING
          text_id = l_textname
        CHANGING
          text    = <ent>-text ).

    ENDLOOP.
    rv_data_kpi-grade1 = rv_data_kpi-grade1 + 1.
    rv_data_kpi-grade2 = rv_data_kpi-grade2 + 1.
    LOOP AT lt_text ASSIGNING <ent>.
      <ent>-index = sy-tabix.
    ENDLOOP.
    rv_data_kpi-togoals = lt_text.

    get_timeline_set( EXPORTING
                        im_reqid = im_reqid_str
                        im_uname = rv_data_kpi-uname
                        im_inits_fl = abap_true
                      CHANGING
                        ch_set = rv_data_kpi-totimeline ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->GET_DELEG_TAB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_UNAME                       TYPE        UNAME
* | [<-->] CH_UNAME_TAB                   TYPE        ZHR_UNAME_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_deleg_tab.
*    DATA ch_uname_tab TYPE TABLE OF hrus_d2.
    CONSTANTS: c_rep TYPE hrus_d2-reppr VALUE 'ZPREM'.
    SELECT us_name AS uname FROM hrus_d2
      INTO TABLE ch_uname_tab
     WHERE rep_name = im_uname
       AND begda <= sy-datum
       AND endda >= sy-datum
       AND reppr = c_rep
       AND active = abap_true.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ZHR_KPI_APP_DPC_EXT=>GET_HISTORY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_REQID                       TYPE        ZHRE_UI_REQID
* | [--->] IV_UNAME                       TYPE        UNAME
* | [--->] IV_DEL                         TYPE        FLAG(optional)
* | [--->] IV_YEAR                        TYPE        GJAHR (default =SY-DATUM+0(4))
* | [<---] ET_ROUTE                       TYPE        ZHR_UI_APPROVER_T
* | [<---] ET_HISTORY_ROUTE               TYPE        ZHR_UI_APPROVER_T
* | [<---] ET_REQUEST                     TYPE        ZHR_UI_KPI_GRA_TT
* | [<---] EV_TOP_UNAME                   TYPE        UNAME
* | [<---] EV_TOP_DELEG                   TYPE        UNAME
* | [<---] EV_DOP_TOP                     TYPE        FLAG
* | [<---] ET_HISTORY                     TYPE        CDREDCD_TAB
* | [<---] ES_UI_KPI_GRA                  TYPE        ZHR_UI_KPI_GRA_S
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_history.

    DATA:
      l_textname       TYPE thead-tdname,
      l_text           TYPE string,
      ls_request       TYPE zhr_ui_kpi_gra_s,
      lt_status        TYPE TABLE OF zhrt_ui_leave_st,

      lv_top_uname     TYPE uname,
      lv_top_deleg     TYPE uname,
      lv_dop_top       TYPE flag,

      lt_history       TYPE TABLE OF cdred,
      lt_route         TYPE zhr_ui_approver_t,
      lt_route_main    TYPE zhr_ui_approver_t,
      lt_history_route TYPE zhr_ui_approver_t,
      lt_request       TYPE TABLE OF zhr_ui_kpi_gra_s.

    DATA(lv_year)   = iv_year.
    DATA(lv_uname)  = iv_uname.
    DATA(lv_reqid)  = iv_reqid.
    DATA(lv_del)    = iv_del.
    DATA(lv_date)   = sy-datum.

* Получаем полный актуальный маршрут согласования
    get_appr_route( EXPORTING
                      im_author = lv_uname
                      iv_year   = lv_year
                     CHANGING
                      ch_tab    = lt_route_main ).

    lt_route = lt_route_main.

    IF lt_route IS NOT INITIAL.
      READ TABLE lt_route WITH KEY top = abap_true ASSIGNING FIELD-SYMBOL(<route>).
      IF sy-subrc EQ 0.

        IF <route>-orgeh IN zcl_tvarvc=>read( i_name = 'ZHR_FIO_GRA_TOP' i_type = 'S' ).
          lv_top_uname = lt_route[ ( lines( lt_route ) - 1 ) ]-uname.
*          DELETE lt_route INDEX ( lines( lt_route ) ).
          READ TABLE lt_route INDEX ( lines( lt_route ) ) ASSIGNING FIELD-SYMBOL(<ls_route_del>).
          IF sy-subrc = 0.
            <ls_route_del>-del = abap_true.
          ENDIF.

          lv_dop_top = abap_true.
        ELSE.

          lv_top_uname = <route>-uname.

        ENDIF.
      ELSE.
        lv_top_uname = lt_route[ lines( lt_route ) ]-uname.
        lv_dop_top = abap_true.

      ENDIF.
    ENDIF.

    DELETE lt_route WHERE del = abap_true.

    lv_top_deleg = zcl_zhr_fio_util=>get_rep_from_hrus_d2( iv_uname = lv_top_uname
                                                           iv_reppr = zcl_zhr_fio_util=>c_reppr-zprem
                                                           iv_date  = lv_date ).

    SELECT SINGLE *
             FROM zhrt_ui_kpi_gra
             INTO CORRESPONDING FIELDS OF ls_request
            WHERE reqid = lv_reqid
              AND uname = lv_uname.
    IF sy-subrc = 0.

      SELECT * FROM zhrt_ui_leave_st INTO TABLE lt_status.

      CALL FUNCTION 'CHANGEDOCUMENT_READ'
        EXPORTING
          objectclass = c_obj_class "'ZHRT_UI_KPI_GRA'"
          objectid    = CONV cdobjectv( lv_reqid )
        TABLES
          editpos     = lt_history
        EXCEPTIONS
          OTHERS      = 1.

      SORT lt_history BY udate DESCENDING utime DESCENDING.
      APPEND ls_request TO lt_request.

      DATA(lr_excl_history) = zcl_tvarvc=>read( i_name = 'ZHR_UI_KPI_EXCL' i_type = 'S' ).
      DATA: lr_excl_changenr TYPE RANGE OF cdchangenr.

      LOOP AT lr_excl_history ASSIGNING FIELD-SYMBOL(<ls_excl_history>).
        APPEND INITIAL LINE TO lr_excl_changenr ASSIGNING FIELD-SYMBOL(<ls_excl_changenr>).
        <ls_excl_changenr> = 'IEQ'.
        <ls_excl_changenr>-low = CONV #( <ls_excl_history>-low ).
      ENDLOOP.

      IF lr_excl_changenr IS INITIAL.
        APPEND INITIAL LINE TO lr_excl_changenr ASSIGNING <ls_excl_changenr>.
        <ls_excl_changenr> = 'IEQ'.
      ENDIF.

      LOOP AT lt_history ASSIGNING FIELD-SYMBOL(<ls_history>) GROUP BY ( key1 = <ls_history>-udate
                                                                         key2 = <ls_history>-utime ).
        CHECK <ls_history>-changenr NOT IN lr_excl_changenr.  " ins NaumovSM

        CLEAR: ls_request-declinecommentary,
               ls_request-percentcommentary.   " ins NaumovSM
        LOOP AT GROUP <ls_history> ASSIGNING FIELD-SYMBOL(<ls_member>).

          ASSIGN COMPONENT <ls_member>-fname OF STRUCTURE ls_request TO FIELD-SYMBOL(<lv_old>).
          IF sy-subrc = 0.
            <lv_old> = <ls_member>-f_old.
          ENDIF.

        ENDLOOP.
        APPEND ls_request TO lt_request.
      ENDLOOP.

      SORT lt_request BY rdate ASCENDING rtime ASCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_request COMPARING rdate rtime.

* По истории собираем историчесткий маршрут
      LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<req>).
        READ TABLE lt_history_route WITH KEY uname = <req>-approver TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
          APPEND VALUE #( uname = <req>-approver ) TO lt_history_route.
        ENDIF.
      ENDLOOP.

* Заполняем комментарий отклонения
      LOOP AT lt_request ASSIGNING <req>.
        CLEAR l_text.
        l_textname = |{ <req>-reqid }D{ <req>-rdate }{ <req>-rtime }|.
        CALL METHOD read_text(
          IMPORTING
            text_id = l_textname
          CHANGING
            text    = l_text ).
        <req>-declinecommentary = l_text.
      ENDLOOP.

* Заполнение таймлана
*    LOOP AT lt_history_route ASSIGNING FIELD-SYMBOL(<ls_history_route_idx>).
      LOOP AT lt_route ASSIGNING  FIELD-SYMBOL(<ls_route_idx>). " WHERE " uname = <ls_history_route_idx>-uname
*                                                                 AND add_fl IS INITIAL
*                                                                   add_fl IS INITIAL
*                                                                  del    IS INITIAL.
        LOOP AT lt_history_route ASSIGNING FIELD-SYMBOL(<ls_history_route_idx>) WHERE uname = <ls_route_idx>-uname
                                                                                  AND idx IS INITIAL.
          <ls_history_route_idx>-idx = <ls_route_idx>-idx.
          EXIT.
        ENDLOOP.
      ENDLOOP.

      DATA: lv_decline_idx TYPE zhr_ui_approver_s-idx
          , lv_prev_idx    TYPE zhr_ui_approver_s-idx
          , ls_next_route  TYPE zhr_ui_approver_s
          , lv_sytabix     TYPE sytabix
          .

*    LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_request_idx>).
* Заполняем основной маршрут, ключевые точки промежутки заполняются позднее
      LOOP AT  lt_request ASSIGNING FIELD-SYMBOL(<ls_request_01>) WHERE aenam    = ls_request-uname
                                                                    AND approver = ls_request-uname.
        <ls_request_01>-idx = '01'.
      ENDLOOP.

      LOOP AT lt_route ASSIGNING <ls_route_idx>. " WHERE
*                                                      ( uname = <ls_request_idx>-aenam
*                                                     OR uname = <ls_request_idx>-deleg )
*                                                  AND add_fl IS INITIAL
*                                                    add_fl IS INITIAL
*                                                   del    IS INITIAL.
        CLEAR lv_decline_idx.
        CLEAR ls_next_route.

        LOOP AT lt_route INTO ls_next_route FROM ( sy-tabix + 1 ).
          EXIT.
        ENDLOOP.

        LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_request_idx>) FROM ( lv_sytabix - 1 ) WHERE ( aenam = <ls_route_idx>-uname
                                                                                                 OR deleg = <ls_route_idx>-uname )
                                                                                              AND approver = ls_next_route-uname
                                                                                              AND idx IS INITIAL.

* запоминаем индекс для считывания следующей записи в request в случае отклонения (проставляем дублирующий индекс)
          lv_sytabix = sy-tabix + 1.

          IF lv_decline_idx IS INITIAL.
            <ls_request_idx>-idx = <ls_route_idx>-idx.
          ELSE.
            <ls_request_idx>-idx = lv_decline_idx - 1.
          ENDIF.
          lv_prev_idx = <ls_request_idx>-idx.

* если было ранее отклонено, то запоминаем индекс для следующей записи.
          IF <ls_request_idx>-declinecommentary IS NOT INITIAL.
            LOOP AT lt_request FROM lv_sytabix ASSIGNING FIELD-SYMBOL(<ls_decline_request>).

              lv_sytabix = sy-tabix + 1.

              IF lv_decline_idx IS NOT INITIAL.

                <ls_decline_request>-idx = lv_decline_idx - 1.
                lv_decline_idx = lv_decline_idx - 1.
              ELSE.

                <ls_decline_request>-idx = <ls_route_idx>-idx - 1.
                lv_decline_idx = <ls_route_idx>-idx - 1.
              ENDIF.
              EXIT.
            ENDLOOP.
          ENDIF.

* если первый статус, то заполняем индексом также вторую запись
          IF <ls_request_idx>-status = '01' OR <ls_request_idx>-declinecommentary IS NOT INITIAL.
            CONTINUE.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.
*        IF sy-subrc <> 0.
*          EXIT.
*        ENDIF.
      ENDLOOP.

*    DATA lv_previous_idx TYPE zhr_ui_kpi_gra_s-idx.

      DATA:      lv_idx_prev      TYPE zhr_ui_kpi_gra_s-idx.

      LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_route_decline>).

        lv_sytabix = sy-tabix.

        IF <ls_route_decline>-idx IS INITIAL.
*        LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_route_previous>) WHERE ( approver = <ls_route_decline>-aenam
*                                                                            OR approver = <ls_route_decline>-deleg )
*                                                                         AND ( aenam = <ls_route_decline>-approver
*                                                                            OR deleg = <ls_route_decline>-approver )
*                                                                         AND idx <> lv_previous_idx.
          IF <ls_route_decline>-aenam = <ls_route_decline>-approver OR
             <ls_route_decline>-deleg = <ls_route_decline>-approver.
*{ Тихонов Г.Ю. 02.06.2021
** { ins NaumovSM
*            READ TABLE lt_route TRANSPORTING NO FIELDS WITH KEY uname = <ls_route_decline>-approver.
*            IF sy-subrc = 0.
** } ins NaumovSM
*              <ls_route_decline>-idx = lv_idx_prev.
** { ins NaumovSM
* { ins NaumovSM
            READ TABLE lt_route INTO DATA(ls_route) WITH KEY uname = <ls_route_decline>-approver.
            IF sy-subrc = 0.
              <ls_route_decline>-idx = ls_route-idx.
*} Тихонов Г.Ю. 02.06.2021
            ELSE.
              ADD 1 TO lv_idx_prev.
              <ls_route_decline>-idx = lv_idx_prev.
            ENDIF.
* } ins NaumovSM
            CONTINUE.
          ENDIF.

          LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_route_previous>)  WHERE ( aenam = <ls_route_decline>-aenam
                                                                               OR deleg = <ls_route_decline>-aenam )
                                                                            AND approver  = <ls_route_decline>-approver
                                                                            AND idx IS NOT INITIAL.
*                                                                                            AND idx = lv_previous_sub.
            IF <ls_route_decline>-declinecommentary IS NOT INITIAL.
              <ls_route_decline>-idx = <ls_route_previous>-idx + 1.
            ELSE.
              <ls_route_decline>-idx = <ls_route_previous>-idx.
            ENDIF.
            lv_idx_prev =  <ls_route_previous>-idx.
*          lv_previous_idx = <ls_route_previous>-idx.
            EXIT.
          ENDLOOP.
          IF sy-subrc <> 0.
*            IF <ls_route_decline>-status = '04'.  " отклонено, сбрасываем на предыдущую
            IF <ls_route_decline>-declinecommentary IS NOT INITIAL.  " отклонено, сбрасываем на предыдущую

              LOOP AT lt_request ASSIGNING <ls_route_previous> WHERE uname = <ls_route_decline>-approver
                                                                 AND idx = lv_idx_prev.
*                <ls_route_decline>-idx = <ls_route_previous>-idx.   " del NaumovSM 29.04.21
*                lv_idx_prev = 1.
                IF <ls_route_previous>-idx - 1 >= 1.
                  <ls_route_decline>-idx = <ls_route_previous>-idx - 1.    " ins NaumovSM 29.04.21
                ELSE.
                  <ls_route_decline>-idx = 1.
                ENDIF.
                lv_idx_prev = <ls_route_decline>-idx.

                EXIT.
              ENDLOOP.

*            Дозаполняем с индекса после 04 - отозвано
              LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_from_same_request>).
                LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_to_same_request>) WHERE uname    = <ls_from_same_request>-uname
                                                                                  AND aenam    = <ls_from_same_request>-aenam
                                                                                  AND approver = <ls_from_same_request>-approver
                                                                                  AND idx      IS INITIAL.
                  <ls_to_same_request>-idx = <ls_from_same_request>-idx.
                  lv_idx_prev = <ls_from_same_request>-idx.
                ENDLOOP.
              ENDLOOP.

              LOOP AT lt_route ASSIGNING <ls_route_idx>. " WHERE del IS INITIAL.
*                LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_request_refresh_idx>) WHERE aenam = <ls_route_idx>-uname
                LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_request_refresh_idx>) WHERE ( aenam = <ls_route_idx>-uname
                                                                                         OR deleg = <ls_route_idx>-uname )
                                                                                          AND idx IS INITIAL.
                  <ls_request_refresh_idx>-idx = <ls_route_idx>-idx.
                  lv_idx_prev = <ls_route_idx>-idx.
                  EXIT.
                ENDLOOP.
              ENDLOOP.

            ELSE.
              DATA(lv_route_idx) = lv_idx_prev + 1.
*              LOOP AT  lt_route TRANSPORTING NO FIELDS FROM lv_route_idx. " WHERE del IS INITIAL.
*                EXIT.
*              ENDLOOP.
*              READ TABLE lt_route INDEX lv_route_idx TRANSPORTING NO FIELDS.   " del NaumovSM
              READ TABLE lt_route INDEX lv_route_idx ASSIGNING FIELD-SYMBOL(<ls_route_decl_idx>).
              IF sy-subrc = 0.
* { ins NaumovSM 30.04.2021
* } ins NaumovSM 30.04.2021
                IF <ls_route_decl_idx>-uname = <ls_route_decline>-aenam.
                  <ls_route_decline>-idx = lv_route_idx.
                  lv_idx_prev = <ls_route_decline>-idx.
                ELSE.
                  <ls_route_decline>-idx = lv_idx_prev + 1.
                  lv_idx_prev = <ls_route_decline>-idx.
                ENDIF.
              ELSE.
* { ins NaumovSM 30.04.2021
                IF lines( lt_route ) < 2.
                  <ls_route_decline>-idx = lv_idx_prev + 1.
                  lv_idx_prev = <ls_route_decline>-idx.
                ELSE.
* } ins NaumovSM 30.04.2021
                  <ls_route_decline>-idx = lv_idx_prev.
                ENDIF.   " ins NaumovSM 30.04.2021
              ENDIF.
            ENDIF.

          ENDIF.
        ELSE.
          lv_idx_prev = <ls_route_decline>-idx.
        ENDIF.
      ENDLOOP.
      IF sy-subrc = 0.

      ENDIF.

    ENDIF.

    IF lv_del IS NOT INITIAL.
      DELETE lt_route_main WHERE del = abap_true.
    ENDIF.

*    LOOP AT lt_request ASSIGNING <ls_request_01> WHERE status = c_st_03 AND deleg IS INITIAL.
*      IF <ls_request_01>-aenam = lv_top_deleg.
*        <ls_request_01>-deleg = <ls_request_01>-aenam.
*      ENDIF.
*    ENDLOOP.

    et_request       = lt_request.
    et_history_route = lt_history_route.
    et_route         = lt_route_main.
    et_history       = lt_history.

    ev_top_uname     = lv_top_uname.
    ev_top_deleg     = lv_top_deleg.
    ev_dop_top       = lv_dop_top.

    es_ui_kpi_gra    = ls_request.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->GET_LEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_UNAME                       TYPE        UNAME
* | [--->] IM_AUTHOR                      TYPE        UNAME(optional)
* | [<-()] RET_UNAME                      TYPE        UNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_leader.
    DATA: l_pernr        TYPE persno,
          ls_lead        TYPE zhr_objbif_objec_leader_s,
          _bipernr       TYPE REF TO zcl_objbif_pernr,
          _biorgeh       TYPE REF TO zhr_objbif_orgeh,
          lt_struc       TYPE TABLE OF struc,
          lt_p0105       TYPE TABLE OF p0105,
          lt_p0001       TYPE TABLE OF p0001,
          l_objid        TYPE objektid,
          l_pernr_author TYPE persno.
    CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
      EXPORTING
        user_name               = im_uname
      IMPORTING
        employeenumber          = l_pernr
      EXCEPTIONS
        no_employeenumber_found = 1
        subtype_not_available   = 2
        OTHERS                  = 3.
    CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
      EXPORTING
        otype  = c_otype_p
        objid  = l_pernr
        datum  = sy-datum
      CHANGING
        result = _bipernr
      EXCEPTIONS
        OTHERS = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    TRY.
        _bipernr->zhr_objbif_pernr~get_orgass(
        EXPORTING
          adatum = sy-datum
          IMPORTING
            resorgeh = _biorgeh
        ).
        _biorgeh->get_leader(
     EXPORTING
       adatum = sy-datum
     IMPORTING
       result = ls_lead
            ).
      CATCH zcx_objbif_objec.
    ENDTRY.

    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = ls_lead-pernr
        infty           = '0105'
        begda           = sy-datum
        endda           = sy-datum
      TABLES
        infty_tab       = lt_p0105
      EXCEPTIONS
        infty_not_found = 1
        invalid_input   = 2
        OTHERS          = 3.
    ret_uname = VALUE #( lt_p0105[ subty = '0001' ]-usrid OPTIONAL ).

    CHECK _biorgeh IS NOT INITIAL.

    IF ls_lead-pernr IS INITIAL OR ls_lead-pernr EQ l_pernr OR ret_uname IS INITIAL.
      CLEAR: lt_struc[].
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype       = c_otype_o
          act_objid       = _biorgeh->objec-objid
          act_wegid       = c_wegid_up
          act_begda       = sy-datum
          act_endda       = sy-datum
          authority_check = ''
        TABLES
          result_struc    = lt_struc
        EXCEPTIONS
          no_plvar_found  = 1
          no_entry_found  = 2
          OTHERS          = 3.
      LOOP AT lt_struc ASSIGNING FIELD-SYMBOL(<struc>).
        CLEAR: _biorgeh, ls_lead.
        m_get_objec c_otype_o <struc>-objid _biorgeh sy-datum.
        CHECK _biorgeh IS NOT INITIAL.
        _biorgeh->get_leader(
         EXPORTING
           adatum = sy-datum
         IMPORTING
           result = ls_lead
                ).
        CHECK ls_lead-pernr IS NOT INITIAL.
        CLEAR lt_p0105[].
        CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
        CALL FUNCTION 'HR_READ_INFOTYPE'
          EXPORTING
            pernr           = ls_lead-pernr
            infty           = '0105'
            begda           = sy-datum
            endda           = sy-datum
          TABLES
            infty_tab       = lt_p0105
          EXCEPTIONS
            infty_not_found = 1
            invalid_input   = 2
            OTHERS          = 3.
        ret_uname = VALUE #( lt_p0105[ subty = '0001' ]-usrid OPTIONAL ).

        CHECK ret_uname IS NOT INITIAL AND ls_lead-pernr NE l_pernr.
        EXIT.
      ENDLOOP.
      CHECK ret_uname IS NOT INITIAL.
      READ TABLE lt_p0105 WITH KEY subty = c_ses INTO DATA(ls_p0105).
      DATA(l_flag) = ls_p0105-zzflag.
      IF l_flag IS INITIAL.
*        send_noses( imper = ls_lead-pernr ).
      ENDIF.
    ENDIF.
    SELECT SINGLE uname3 FROM zhrt_ui_topman INTO @DATA(l_uname) WHERE uname1 = @im_uname.
    IF sy-subrc IS INITIAL.
      ret_uname = l_uname.
    ENDIF.
*    SELECT SINGLE uname3 FROM zhrt_ui_topman INTO @DATA(l_uname) WHERE uname1 = @im_uname AND uname2 IS INITIAL.
*    IF sy-subrc IS INITIAL.
*      ret_UNAME = l_uname.
*    ENDIF.

* Обработка "перодо мной" и "после меня"
    SELECT * FROM zhrt_ui_approver INTO TABLE @DATA(lt_approver) WHERE approver = @ret_uname OR approver = @im_uname.
    IF sy-subrc IS INITIAL.
      TRY.
          ret_uname = lt_approver[ approver = im_uname queue = c_queue_after ]-add_approver.
          EXIT.
        CATCH cx_root.

      ENDTRY.
      CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
        EXPORTING
          user_name               = im_author
        IMPORTING
          employeenumber          = l_pernr_author
        EXCEPTIONS
          no_employeenumber_found = 1
          subtype_not_available   = 2
          OTHERS                  = 3.
      m_read_inf l_pernr_author '0001' lt_p0001 sy-datum.   " ins доработка даты NaumovSM
*      m_read_inf l_pernr_author '0001' lt_p0001.   " del доработка даты NaumovSM
      l_objid = VALUE #( lt_p0001[ 1 ]-orgeh ).
      CLEAR: lt_struc[].
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype       = c_otype_o
          act_objid       = l_objid
          act_wegid       = c_wegid_up
          act_begda       = sy-datum
          act_endda       = sy-datum
          authority_check = ''
        TABLES
          result_struc    = lt_struc
        EXCEPTIONS
          no_plvar_found  = 1
          no_entry_found  = 2
          OTHERS          = 3.
      LOOP AT lt_approver ASSIGNING FIELD-SYMBOL(<appr>) WHERE queue = c_queue_before.
        TRY.
            DATA(ls_struc) = lt_struc[ objid = <appr>-orgeh ].
          CATCH cx_root.
            CONTINUE.
        ENDTRY.
        ret_uname = <appr>-add_approver.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->GET_OE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_PLANS                       TYPE        PLANS
* | [<-()] RET_ORGEH                      TYPE        ORGEH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_oe.
    DATA: it_1001 TYPE TABLE OF p1001.
    DATA: it_objects TYPE TABLE OF hrobject,
          wa_objects TYPE hrobject.

    CLEAR: wa_objects.
    REFRESH it_objects.
    wa_objects-plvar = c_plvar.
    wa_objects-otype = 'S'.
    wa_objects-objid = im_plans.
    APPEND wa_objects TO it_objects.

* Чтение 1001 ИТ
    REFRESH it_1001[].
    CALL FUNCTION 'RH_READ_INFTY_1001'
      EXPORTING
*       istat           = '1'
*       subty           = 'AZ02'
        begda           = sy-datum
        endda           = sy-datum
        with_stru_auth  = ''
      TABLES
        objects         = it_objects[]
        i1001           = it_1001[]
      EXCEPTIONS
        nothing_found   = 1
        wrong_condition = 2
        OTHERS          = 3.

    LOOP AT it_1001[] ASSIGNING FIELD-SYMBOL(<ls_1001>) WHERE rsign EQ 'A'
                                                          AND relat EQ '012'
                                                          AND sclas EQ 'O'.
      ret_orgeh = CONV objektid( <ls_1001>-sobid ).
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->GET_OE_ADMIN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_ORGEH                       TYPE        ORGEH
* | [<---] EX_PERNR                       TYPE        PERSNO
* | [<---] EX_ORGEH                       TYPE        ORGEH
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_oe_admin.
    DATA: orgeh    TYPE orgeh,
          _struc   TYPE TABLE OF struc,
          ls_lead  TYPE zhr_objbif_objec_leader_s,
          _biorgeh TYPE REF TO zhr_objbif_orgeh.
    DEFINE m_get_objec.
      CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
        EXPORTING
          otype  = &1
          objid  = CONV objektid( &2 )
*          datum  = sy-datum   " del NaumovSM
          datum  = &4   " ins NaumovSM
        CHANGING
          result = &3
        EXCEPTIONS
          OTHERS = 4.
    END-OF-DEFINITION.
    DEFINE m_read_inf.
      REFRESH: &3.
      CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          pernr           = CONV persno( &1 )
          infty           = &2
          begda           = &4   " ins NaumovSM
          endda           = &4   " ins NaumovSM
*          begda           = sy-datum   " del NaumovSM
*          endda           = sy-datum   " del NaumovSM
        TABLES
          infty_tab       = &3
        EXCEPTIONS
          infty_not_found = 1
          invalid_input   = 2
          OTHERS          = 3.
    END-OF-DEFINITION.
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype       = c_otype_o
        act_objid       = im_orgeh
        act_wegid       = 'ORGA-UP'
        act_plvar       = c_plvar
        act_begda       = sy-datum
        act_endda       = sy-datum
        authority_check = ''
      TABLES
        result_struc    = _struc
      EXCEPTIONS
        no_plvar_found  = 1
        no_entry_found  = 2
        OTHERS          = 3.

    SORT _struc BY level DESCENDING.
    READ TABLE _struc INDEX 1 INTO DATA(ls_struc).

    ex_orgeh = ls_struc-objid.

    CLEAR: _biorgeh, ls_lead.
*    m_get_objec c_otype_o ex_orgeh _biorgeh.   " del NaumovSM
    m_get_objec c_otype_o ex_orgeh _biorgeh sy-datum.   " ins NaumovSM
    CHECK _biorgeh IS NOT INITIAL.
    _biorgeh->get_leader(
     EXPORTING
       adatum = sy-datum
     IMPORTING
       result = ls_lead
            ).

    ex_pernr = ls_lead-pernr.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->GET_OLD_APPROVER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_UNAME                       TYPE        UNAME
* | [--->] IM_REQID                       TYPE        ZHRE_UI_REQID
* | [<-()] RET_UNAME                      TYPE        UNAME
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_old_approver.
    DATA: lt_history TYPE TABLE OF cdred,
          lt_request TYPE TABLE OF zhrt_ui_kpi_gra.
*          ls_request TYPE zhrt_ui_kpi_gra.
    SELECT SINGLE *
             FROM zhrt_ui_kpi_gra
             INTO @DATA(ls_request)
            WHERE reqid = @im_reqid AND uname = @im_uname.
    CHECK sy-subrc = 0.

    CALL FUNCTION 'CHANGEDOCUMENT_READ'
      EXPORTING
        objectclass = c_obj_class
        objectid    = CONV cdobjectv( im_reqid )
      TABLES
        editpos     = lt_history
      EXCEPTIONS
        OTHERS      = 1.

    SORT lt_history BY udate DESCENDING utime DESCENDING.

    APPEND ls_request TO lt_request.

    LOOP AT lt_history ASSIGNING FIELD-SYMBOL(<ls_history>) GROUP BY ( key1 = <ls_history>-udate
                                                                       key2 = <ls_history>-utime ).
      LOOP AT GROUP <ls_history> ASSIGNING FIELD-SYMBOL(<ls_member>).

        ASSIGN COMPONENT <ls_member>-fname OF STRUCTURE ls_request TO FIELD-SYMBOL(<lv_old>).
        IF sy-subrc = 0.
          <lv_old> = <ls_member>-f_old.
        ENDIF.

      ENDLOOP.
      APPEND ls_request TO lt_request.
    ENDLOOP.

    SORT lt_request BY rdate DESCENDING rtime DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_request COMPARING rdate rtime.

    CLEAR ls_request.
    READ TABLE lt_request INDEX 1 INTO ls_request.
    CHECK ls_request-aenam EQ sy-uname.
    CLEAR ls_request.
    READ TABLE lt_request INDEX 2 INTO ls_request.
    ret_uname = ls_request-aenam.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->GET_PERSON_SET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET
* | [--->] IM_ADM_FLAG                    TYPE        FLAG(optional)
* | [<-->] CH_SET                         TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_PERSON(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_person_set.
    TYPES: BEGIN OF t_podr,
             orgeh TYPE orgeh,
             pernr TYPE persno,
           END OF t_podr.
    DATA: pernr         TYPE persno,
          lt_p0001      TYPE TABLE OF p0001,
          _objec        TYPE TABLE OF objec,
          _struc        TYPE TABLE OF struc,
          lt_struc      LIKE _struc,
          lt_struc_in   LIKE _struc,
          orgeh         TYPE orgeh,
          _bipernr      TYPE REF TO zhr_objbif_pernr,
          _biplans      TYPE REF TO zhr_objbif_plans,
          _biorgeh      TYPE REF TO zhr_objbif_orgeh,
          ls_lead       TYPE zhr_objbif_objec_leader_s,
          ls_lead_in    TYPE zhr_objbif_objec_leader_s,
          ls_lead_after TYPE zhr_objbif_objec_leader_s,
          l_del_lev     TYPE i VALUE '0',
          l_per_lead    TYPE persno VALUE '99999998',
          l_level       TYPE i,
          lt_podr       TYPE TABLE OF t_podr,
          lt_p0105      TYPE TABLE OF p0105,
          lt_p0002      TYPE TABLE OF p0002,
          r_uname       TYPE RANGE OF uname,
          l_approver    TYPE uname,
          l_pernr       TYPE persno,
          l_sendler     TYPE uname,
          l_uname       TYPE uname,
          lt_deleg      TYPE zhr_uname_t,
          lf_lead       TYPE flag.
* { ins NaumovSM проверка на уволенного
    DATA: lt_hire_fire_pernr TYPE TABLE OF phifi
        , lt_history         TYPE TABLE OF cdred
        .

    DEFINE m_get_objec.
      CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
        EXPORTING
          otype  = &1
          objid  = CONV objektid( &2 )
*          datum  = sy-datum   " del NaumovSM
          datum  = &4   " ins NaumovSM
        CHANGING
          result = &3
        EXCEPTIONS
          OTHERS = 4.
    END-OF-DEFINITION.
    DEFINE m_read_inf.
      REFRESH: &3.
      CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          pernr           = CONV persno( &1 )
          infty           = &2
*          begda           = l_date"sy-datum   " del NaumovSM
*          endda           = l_date"sy-datum   " del NaumovSM
          begda           = &4   " ins NaumovSM
          endda           = &4   " ins NaumovSM
        TABLES
          infty_tab       = &3
        EXCEPTIONS
          infty_not_found = 1
          invalid_input   = 2
          OTHERS          = 3.
    END-OF-DEFINITION.

    DATA: lt_status TYPE TABLE OF zhrt_ui_leave_st.
    SELECT * FROM zhrt_ui_leave_st
      INTO TABLE lt_status.


    DATA: l_year TYPE gjahr,
          l_date TYPE dats.
    DATA(lt_keys) = io_tech_request_context->get_source_keys( ).
    l_uname = VALUE #( lt_keys[ name = 'UNAME' ]-value OPTIONAL ).
    l_year = VALUE #( lt_keys[ name = 'YEAR' ]-value OPTIONAL ).
    DATA(l_adm) = VALUE #( lt_keys[ name = 'ADMIN' ]-value OPTIONAL ).
    l_date = |{ l_year }1231|.
*    l_year = sy-datum+0(4).
    IF l_uname IS INITIAL.
      l_uname = sy-uname.
    ENDIF.
    get_deleg_tab( EXPORTING im_uname = l_uname
                    CHANGING ch_uname_tab = lt_deleg ).
    CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
      EXPORTING
        user_name               = l_uname
      IMPORTING
        employeenumber          = pernr
      EXCEPTIONS
        no_employeenumber_found = 1
        subtype_not_available   = 2
        OTHERS                  = 3.

* { ins NaumovSM Добавляем анализ на уволенность сотрудника (для просмотра в будущем)
    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HRF_HIRE_FIRE'
      EXPORTING
        pernr         = l_pernr
*       BEGDA         = '18000101'
*       ENDDA         = '99991231'
*       LANGUAGE      =
*       PERSON        =
      TABLES
        ms_data       = lt_hire_fire_pernr
      EXCEPTIONS
        reject_pernr  = 1
        display_error = 2
        OTHERS        = 3.
    IF sy-subrc = 0.
      SORT lt_hire_fire_pernr DESCENDING BY endda.
      LOOP AT lt_hire_fire_pernr ASSIGNING FIELD-SYMBOL(<ls_hire_fire>)
                                     WHERE stat2 = '3'.
        l_date = <ls_hire_fire>-endda.
        EXIT.
      ENDLOOP.
* Implement suitable error handling here
    ENDIF.
* } ins NaumovSM

*    m_read_inf pernr '0001' lt_p0001.
    m_read_inf pernr '0001' lt_p0001 l_date.
    TRY.
        orgeh = get_oe( im_plans = lt_p0001[ 1 ]-plans ).
      CATCH cx_root.
        EXIT.
    ENDTRY.

* Для администратора заменяем корневую ОЕ и ТН
    IF l_adm EQ abap_true.
*      orgeh = VALUE #( lt_p0001[ 1 ]-orgeh OPTIONAL ).
*      get_oe_admin( EXPORTING
*                      im_orgeh = orgeh
*                    IMPORTING
*                      ex_pernr = l_pernr
*                      ex_orgeh = DATA(l_orgeh)
*                    ).
*      pernr = l_pernr.
*      orgeh = l_orgeh.
      pernr = '99999999'.
      orgeh = '01000001'.
    ENDIF.

    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype       = c_otype_o
        act_objid       = orgeh
        act_wegid       = 'O-O_DOWN' "c_wegid_o
        act_plvar       = c_plvar
        act_begda       = l_date
        act_endda       = l_date
        authority_check = ''
      TABLES
        result_struc    = _struc
      EXCEPTIONS
        no_plvar_found  = 1
        no_entry_found  = 2
        OTHERS          = 3.

    l_level = '1'.
    DO 5 TIMES.
      IF sy-index NE 1.
        ADD 1 TO l_del_lev.
      ENDIF.
      LOOP AT _struc ASSIGNING FIELD-SYMBOL(<struc>) WHERE level = l_level.
        CLEAR: lt_struc[], lf_lead.
        CALL FUNCTION 'RH_STRUC_GET'
          EXPORTING
            act_otype       = c_otype_o
            act_objid       = <struc>-objid
            act_wegid       = 'OSP'
            act_plvar       = c_plvar
            act_begda       = l_date
            act_endda       = l_date
            authority_check = ''
          TABLES
            result_struc    = lt_struc
          EXCEPTIONS
            no_plvar_found  = 1
            no_entry_found  = 2
            OTHERS          = 3.
        ls_lead_after = ls_lead.
        CLEAR: _biorgeh, ls_lead.
        m_get_objec c_otype_o <struc>-objid _biorgeh l_date.
        IF _biorgeh IS NOT INITIAL.
          _biorgeh->get_leader(
                       EXPORTING
                         adatum = l_date
                       IMPORTING
                         result = ls_lead
                              ).
        ENDIF.




        IF ls_lead-pernr IS INITIAL.
          APPEND INITIAL LINE TO lt_struc ASSIGNING FIELD-SYMBOL(<new_lead>).
          <new_lead>-objid = ls_lead-pernr = l_per_lead.
          IF <struc>-objid EQ '01000001'.
            <new_lead>-objid = ls_lead-pernr = l_per_lead = pernr.
          ENDIF.
          <new_lead>-otype = c_otype_p.
          l_per_lead = l_per_lead - 1.
        ENDIF.

        APPEND VALUE #( orgeh = <struc>-objid pernr = ls_lead-pernr ) TO lt_podr.
        DELETE lt_struc WHERE otype NE c_otype_p.
        READ TABLE lt_struc WITH KEY objid = ls_lead-pernr TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
*          READ TABLE lt_struc INDEX 1 INTO DATA(ls_struc).
*          ls_struc-objid = ls_lead-pernr.
*          APPEND ls_struc TO lt_struc.
          lf_lead = abap_true.
          READ TABLE ch_set WITH KEY id = ls_lead-pernr TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
            CLEAR lf_lead.
          ENDIF.
        ENDIF.
        LOOP AT lt_struc ASSIGNING FIELD-SYMBOL(<in_struc>) WHERE otype = c_otype_p.
          APPEND INITIAL LINE TO ch_set ASSIGNING FIELD-SYMBOL(<entity>).
          <entity>-id = <in_struc>-objid.
          <entity>-hierarchylevel = <struc>-level + <struc>-level.
          IF lf_lead IS INITIAL.
            <entity>-parentid = ls_lead-pernr.
*            IF <entity>-id = <entity>-parentid.
*              <entity>-parentid = ls_lead_after-pernr.
*            ENDIF.
          ELSE.
            <entity>-parentid = pernr.
          ENDIF.
          <entity>-group = <struc>-objid.
          <entity>-collapsed = 'leaf'.
*          m_read_inf <entity>-id '0105' lt_p0105.
          m_read_inf <entity>-id '0105' lt_p0105 l_date.
          <entity>-uname = VALUE #( lt_p0105[ subty = '0001' ]-usrid OPTIONAL ).
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <entity>-uname ) TO r_uname.
          CHECK <in_struc>-objid EQ ls_lead-pernr.
          <entity>-hierarchylevel = <entity>-hierarchylevel - 1.
          IF lines( lt_struc[] ) > 1.
            <entity>-collapsed = 'expanded'.
          ENDIF.

          IF <entity>-id = <entity>-parentid.
            CLEAR: lt_struc_in[].
            CALL FUNCTION 'RH_STRUC_GET'
              EXPORTING
                act_otype       = c_otype_o
                act_objid       = <struc>-objid
                act_wegid       = 'ORGA-UP' "c_wegid_up
                act_begda       = l_date
                act_endda       = l_date
                authority_check = ''
              TABLES
                result_struc    = lt_struc_in
              EXCEPTIONS
                no_plvar_found  = 1
                no_entry_found  = 2
                OTHERS          = 3.
            DELETE lt_struc_in WHERE objid = <struc>-objid.
            LOOP AT lt_struc_in ASSIGNING FIELD-SYMBOL(<struc_up>).
              EXIT.
            ENDLOOP.
            IF <struc_up> IS ASSIGNED. "10.12.2019 AVKUPTSOV3
              <entity>-parentid = VALUE #( lt_podr[ orgeh = <struc_up>-objid ]-pernr OPTIONAL ).
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDLOOP.
      l_level = l_level + 1.
    ENDDO.


    READ TABLE ch_set ASSIGNING <entity> WITH KEY id = pernr.
    IF sy-subrc NE 0.
      APPEND VALUE #( id = pernr uname = l_uname year = l_year reqid = '1' hierarchylevel = '1' collapsed = 'expanded' ) TO ch_set.
*<--10.12.2019 AVKUPTSOV3 fs assigning fix
    ELSE.
      CLEAR: <entity>-parentid, <entity>-group.
      <entity>-collapsed = 'expanded'.
    ENDIF.
*10.12.2019 AVKUPTSOV3-->

    SORT r_uname BY low.
    DELETE ADJACENT DUPLICATES FROM r_uname COMPARING low.
    SELECT uname, procent, status, yeara, percentcommentary, approver, aenam, reqid
      FROM zhrt_ui_kpi_gra
      INTO TABLE @DATA(lt_req)
     WHERE uname IN @r_uname
       AND yeara EQ @l_year.
************************************************
* Добавляем ТН у которых нет подчинения по структуре
    DATA: l_persno TYPE persno.
    SELECT uname, procent, status, yeara, percentcommentary, approver, aenam, reqid
      FROM zhrt_ui_kpi_gra AS gra
      INTO TABLE @DATA(lt_req_down)
     WHERE ( approver = @l_uname OR ( aenam = @l_uname AND uname NE @l_uname ) )
       AND yeara EQ @l_year.
    LOOP AT lt_req_down ASSIGNING FIELD-SYMBOL(<req_down>) WHERE status NE c_st_06.
*      READ TABLE ch_set WITH KEY reqid = <req_down>-reqid TRANSPORTING NO FIELDS.
      READ TABLE ch_set WITH KEY uname = <req_down>-uname TRANSPORTING NO FIELDS.
      CHECK sy-subrc NE 0.
      APPEND INITIAL LINE TO ch_set ASSIGNING <entity>.
      CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
        EXPORTING
          user_name               = <req_down>-uname
        IMPORTING
          employeenumber          = l_persno
        EXCEPTIONS
          no_employeenumber_found = 1
          subtype_not_available   = 2
          OTHERS                  = 3.

      <entity>-id = l_persno.
      <entity>-uname = <req_down>-uname.
      <entity>-reqid = <req_down>-reqid.
      <entity>-parentid = pernr.
      <entity>-hierarchylevel = 3.
      <entity>-status = <req_down>-status.
      <entity>-collapsed = 'leaf'.
    ENDLOOP.
    APPEND LINES OF lt_req_down TO lt_req.
************************************************
    DATA: l_count TYPE zhrt_ui_kpi_gra-reqid VALUE '1'.
    LOOP AT ch_set ASSIGNING <entity>.
      <entity>-reqid = VALUE #( lt_req[ uname = <entity>-uname ]-reqid OPTIONAL ).
      <entity>-crossflag = abap_true.
      <entity>-hierarchylevel = <entity>-hierarchylevel - 1.
      CLEAR: _bipernr, _biplans, _biorgeh.
*      m_get_objec c_otype_p <entity>-id _bipernr.   " del NaumovSM
      m_get_objec c_otype_p <entity>-id _bipernr l_date.   " ins NaumovSM
      IF _bipernr IS NOT INITIAL.
        <entity>-ename = _bipernr->get_name( adatum = l_date anoauth = abap_true ).
      ELSE.
        <entity>-ename = 'Вакансия'.
      ENDIF.
      TRY.
          _bipernr->get_orgass(
            EXPORTING
              adatum = l_date
            IMPORTING
              resplans = _biplans
              resorgeh = _biorgeh
        ).
*         Наименование должности
          IF _biplans IS NOT INITIAL.
            <entity>-plnam = _biplans->get_name( adatum = l_date anoauth = abap_true ).
            TRANSLATE <entity>-plnam+0(1) TO UPPER CASE.
          ENDIF.
          IF _biorgeh IS NOT INITIAL.
            <entity>-orgna = <entity>-podrname = _biorgeh->get_name( adatum = l_date anoauth = abap_true ).
************************************************************************************************
*            er_entity-podrname = _biorgeh->get_name( adatum = sy-datum ).
            DATA: _orgpath  TYPE zhr_objbif_objec_orgpath_t,
                  it_attrib TYPE TABLE OF pt1222.
            _biorgeh->get_orgpath(
                       EXPORTING
                         aworigin = 'X'
*                             aminlevel = 3
                         adatum = l_date
                       IMPORTING
                         result = _orgpath[]
                       ).
            SORT _orgpath BY pobid DESCENDING.
            LOOP AT _orgpath ASSIGNING FIELD-SYMBOL(<path>).
              CLEAR _biorgeh.
              CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
                EXPORTING
                  otype  = c_otype_o
                  objid  = <path>-pobid
                  datum  = l_date
                CHANGING
                  result = _biorgeh
                EXCEPTIONS
                  OTHERS = 1.
              CLEAR: it_attrib[].
              CALL FUNCTION 'RH_OM_ATTRIBUTES_READ'
                EXPORTING
                  plvar            = c_plvar
                  otype            = c_otype_o
                  objid            = <path>-pobid
                  seldate          = l_date
                TABLES
                  attrib           = it_attrib
                EXCEPTIONS
                  no_active_plvar  = 1
                  no_attributes    = 2
                  no_values        = 3
                  object_not_found = 4
                  OTHERS           = 5.
              IF <entity>-dirname IS INITIAL.
                READ TABLE it_attrib ASSIGNING FIELD-SYMBOL(<attr>) WITH KEY attrib = 'ZLEVEL' low = 'УП'.
                IF sy-subrc EQ 0.
                  <entity>-dirname = _biorgeh->get_name( adatum = l_date anoauth = abap_true ).
                ENDIF.
              ENDIF.
              IF <entity>-podrname IS INITIAL.
                READ TABLE it_attrib ASSIGNING <attr> WITH KEY attrib = 'ZLEVEL' low = 'ДП'.
                IF sy-subrc EQ 0.
                  <entity>-podrname = _biorgeh->get_name( adatum = l_date anoauth = abap_true ).
                ENDIF.
              ENDIF.
            ENDLOOP.
************************************************************************************************
          ENDIF.
        CATCH cx_root.
      ENDTRY.
      <entity>-procent = VALUE #( lt_req[ uname = <entity>-uname ]-procent OPTIONAL ).
      <entity>-status = VALUE #( lt_req[ uname = <entity>-uname ]-status OPTIONAL ).
      <entity>-stnam = VALUE #( lt_status[ ident = <entity>-status ]-stnam OPTIONAL ).
      TRY.
          <entity>-year = lt_req[ uname = <entity>-uname ]-yeara.
        CATCH cx_root.
          <entity>-year = sy-datum+0(4).
      ENDTRY.
      CLEAR l_approver.
      l_approver = VALUE #( lt_req[ uname = <entity>-uname ]-approver OPTIONAL ).
      l_sendler = VALUE #( lt_req[ uname = <entity>-uname ]-aenam OPTIONAL ).
      <entity>-percentcommentary = VALUE #( lt_req[ uname = <entity>-uname ]-percentcommentary OPTIONAL ).


      IF l_approver IS NOT INITIAL.
        CLEAR l_pernr.
        CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
          EXPORTING
            user_name               = l_approver
          IMPORTING
            employeenumber          = l_pernr
          EXCEPTIONS
            no_employeenumber_found = 1
            subtype_not_available   = 2
            OTHERS                  = 3.
*        m_read_inf l_pernr '0002' lt_p0002.
        m_read_inf l_pernr '0002' lt_p0002 l_date.
        IF lt_p0002[] IS NOT INITIAL.
          <entity>-approver_fio = |{ lt_p0002[ 1 ]-nachn } { lt_p0002[ 1 ]-vorna+0(1) }. { lt_p0002[ 1 ]-midnm+0(1) }.|.
        ENDIF.
      ENDIF.
*      READ TABLE lt_deleg With key table_line = l_approver TRANSPORTING NO FIELDS.
*      IF sy-subrc eq 0.
*        DATA(lf_apr) = abap_true.
*      ENDIF.
*      READ TABLE lt_deleg With key table_line = l_sendler TRANSPORTING NO FIELDS.
*      IF sy-subrc eq 0.
*        DATA(lf_sen) = abap_true.
*      ENDIF.
      IF <entity>-status NE '04'.
        IF l_approver NE l_uname.
          CLEAR <entity>-crossflag.
*        IF lf_apr eq abap_true.
*          <entity>-crossflag = abap_true.
*          <entity>-accessappr = abap_true.
*        ENDIF.
        ENDIF.
        IF l_sendler EQ l_uname."OR l_sendler EQ l_user..
          <entity>-crossflag = abap_true.
          <entity>-accessback = abap_true.
        ENDIF.
        IF l_approver EQ l_uname.
          <entity>-accessappr = abap_true.
        ENDIF.
      ENDIF.
      IF <entity>-status EQ '03'.
        IF l_sendler EQ l_uname OR l_approver EQ l_uname OR l_sendler EQ sy-uname OR l_approver EQ sy-uname.
          <entity>-crossflag = abap_true.
          <entity>-accessback = abap_true.
          <entity>-accessappr = abap_false.
        ENDIF.
      ENDIF.
      IF <entity>-status EQ c_st_06.
        <entity>-crossflag = abap_true.
      ENDIF.

* <-- ins NaumovSM 15.12.20
      IF <entity>-status                   EQ c_st_03 AND
         check_admin( im_uname = l_uname ) EQ abap_true.
        <entity>-accessappr = abap_true.
      ENDIF.
* --> ins NaumovSM 15.12.20

    ENDLOOP.



    LOOP AT ch_set ASSIGNING <entity>.
      IF <entity>-reqid IS INITIAL.
        DO."30 TIMES.
          READ TABLE ch_set WITH KEY reqid = l_count TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            <entity>-reqid = l_count.
            EXIT.
          ELSE.
            l_count = l_count + 1.
          ENDIF.
        ENDDO.
      ENDIF.
      CONDENSE <entity>-procent NO-GAPS.
      READ TABLE ch_set WITH KEY id = <entity>-parentid ASSIGNING FIELD-SYMBOL(<ent>).
      CHECK sy-subrc EQ 0.
      <ent>-collapsed = 'expanded'.
    ENDLOOP.

    LOOP AT ch_set ASSIGNING <entity>.
      READ TABLE ch_set WITH KEY id = <entity>-parentid TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        <entity>-parentid = pernr.
      ENDIF.
    ENDLOOP.
    IF l_adm EQ abap_true.
      LOOP AT ch_set ASSIGNING <entity>.
        <entity>-crossflag = abap_true.
      ENDLOOP.
    ENDIF.
*    DELETE ch_set WHERE hierarchylevel > 0.
    SORT ch_set BY hierarchylevel ASCENDING.

    LOOP AT ch_set ASSIGNING <ent>.
      LOOP AT ch_set ASSIGNING FIELD-SYMBOL(<ent2>).

      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->GET_REPOSITORY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_REPOSITORY                  TYPE        SAEARCHIVI
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
    METHOD get_repository.
      SELECT SINGLE archiv_id
        FROM toaom
        INTO rv_repository
       WHERE sap_object = 'ARCHIVE'
         AND ar_object = 'ZF_REQ'
         AND ar_status = 'X'.
    ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ZHR_KPI_APP_DPC_EXT=>GET_TIMELINE_SET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_REQID                       TYPE        ZHRE_UI_REQID
* | [--->] IM_UNAME                       TYPE        UNAME
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [--->] IM_INITS_FL                    TYPE        FLAG(optional)
* | [<-->] CH_SET                         TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_TIMELINE
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_timeline_set.
    DATA:
*          lt_history       TYPE TABLE OF cdred,
      lt_request        TYPE zhr_ui_kpi_gra_tt, "zhrt_ui_kpi_gra,
      ls_prew_req       LIKE LINE OF lt_request,
      l_reqid           TYPE zhrt_ui_kpi_gra-reqid,
      l_uname           TYPE uname,
      pernr             TYPE persno,
      _bipernr          TYPE REF TO zhr_objbif_pernr,
      _biplans          TYPE REF TO zhr_objbif_plans,
*          l_textname        TYPE thead-tdname,
      l_collapsed       TYPE zhre_ui_collapsed_text,
*          l_text           TYPE string,
      lt_p0002          TYPE TABLE OF p0002,
      lt_p0002_ws       TYPE TABLE OF p0002,
      l_idx             TYPE i,
      lt_route          TYPE zhr_ui_approver_t,
      lt_history_route  TYPE zhr_ui_approver_t,
      l_idx_appr1       TYPE i,
      l_idx_appr2       TYPE i,
      l_idx_route       TYPE i,
      lv_idx_next       TYPE i,
      l_appr            TYPE uname,
      l_aenam           TYPE uname,
      l_status          TYPE zhr_ident_st,
      l_top_uname       TYPE uname,
      l_zam_uname       TYPE uname,
      l_deleg_uname     TYPE uname,
      ls_request        TYPE zhr_ui_kpi_gra_s, "zhrt_ui_kpi_gra,
*          lt_status        TYPE TABLE OF zhrt_ui_leave_st,

      lt_appr_deleg     TYPE zhr_uname_t,
      lv_top_deleg      TYPE uname,
      lv_appr_deleg     TYPE uname,
      lv_approver       TYPE uname,
      lv_dop_top        TYPE flag,
      lv_role           TYPE zhre_ui_role_empl,
      lt_route_main_all TYPE zhr_ui_approver_t_all,
      ls_route_main_all LIKE LINE OF lt_route_main_all,
      l_txt_route       TYPE string,
      l_pernr           TYPE persno,
      lv_text_add_set   TYPE string,
      lv_req            LIKE LINE OF ch_set.

    FIELD-SYMBOLS: <after>  LIKE LINE OF ch_set,
                   <entity> LIKE LINE OF ch_set.
    CONSTANTS: c_01 TYPE zhr_ident_st VALUE '01',
               c_02 TYPE zhr_ident_st VALUE '02',
               c_03 TYPE zhr_ident_st VALUE '03',
               c_04 TYPE zhr_ident_st VALUE '04',
               c_05 TYPE zhr_ident_st VALUE '05',
               c_06 TYPE zhr_ident_st VALUE '06',
               c_07 TYPE zhr_ident_st VALUE '07',
               c_08 TYPE zhr_ident_st VALUE '08',
               c_09 TYPE zhr_ident_st VALUE '09',
               c_10 TYPE zhr_ident_st VALUE '10',
               c_11 TYPE zhr_ident_st VALUE '11'.

    DEFINE m_add_set.
*      LOOP AT ch_set ASSIGNING <entity> FROM lines( ch_set ) WHERE status_text = &2
*                                                               AND username    = &3.
*        EXIT.
*      ENDLOOP.
*      IF sy-subrc <> 0.
      APPEND INITIAL LINE TO ch_set ASSIGNING <entity>.
*        ENDIF.
      <entity>-date = |{ &1-rdate+6(2) }.{ &1-rdate+4(2) }.{ &1-rdate+0(4) }|.
      <entity>-time = |{ &1-rtime+0(2) }:{ &1-rtime+2(2) }:{ &1-rtime+4(2) }|.
      <entity>-status_text = &2.
      <entity>-status = &5.
      <entity>-username = &3.
      <entity>-role = &4.
      <entity>-commentary = &1-declinecommentary.
*   { ins NaumovSM 11.03.2021 7700148647
      IF &1-deleg IS NOT INITIAL AND
         &2 <> 'На согласовании' AND
         &2 <> 'Доработка' AND
         &2 <> 'Доработал(а) отчёт'.
        <entity>-deleg    = &1-deleg.
      ENDIF.
*   } ins NaumovSM 11.03.2021 7700148647
    END-OF-DEFINITION.

    DEFINE m_ins_set.
*      LOOP AT ch_set ASSIGNING <entity> FROM lines( ch_set ) WHERE status_text = &2
*                                                               AND username    = &3.
*        EXIT.
*      ENDLOOP.
*      IF sy-subrc <> 0.
      INSERT INITIAL LINE INTO ch_set ASSIGNING <entity>.
*        ENDIF.
      <entity>-date = |{ &1-rdate+6(2) }.{ &1-rdate+4(2) }.{ &1-rdate+0(4) }|.
      <entity>-time = |{ &1-rtime+0(2) }:{ &1-rtime+2(2) }:{ &1-rtime+4(2) }|.
      <entity>-status_text = &2.
      <entity>-status = &5.
      <entity>-username = &3.
      <entity>-role = &4.
      <entity>-commentary = &1-declinecommentary.
*   { ins NaumovSM 11.03.2021 7700148647
      IF &1-deleg IS NOT INITIAL AND
         &2 <> 'На согласовании' AND
         &2 <> 'Доработка' AND
         &2 <> 'Доработал(а) отчёт'.
        <entity>-deleg    = &1-deleg.
      ENDIF.
*   } ins NaumovSM 11.03.2021 7700148647
    END-OF-DEFINITION.

    DEFINE m_add_set_witout_s.
      INSERT INITIAL LINE INTO ch_set ASSIGNING <entity>.
      IF &1-rdate IS NOT INITIAL.
        <entity>-date = |{ &1-rdate+6(2) }.{ &1-rdate+4(2) }.{ &1-rdate+0(4) }|.
        <entity>-time = |{ &1-rtime+0(2) }:{ &1-rtime+2(2) }:{ &1-rtime+4(2) }|.
      ENDIF.
      <entity>-status_text = &2.

      IF &3 IS NOT INITIAL.
*        <entity>-commentary = <entity>-commentary && 'Предыдущий согласующий:' && ` ` && &3 && ` ` && &4 && ` ` && &5.
*        <entity>-commentary = <entity>-commentary && ` ` && 'Новый согласующий:' && ` ` && &6.
        <entity>-prev_aprov = 'Предыдущий согласующий:' && ` ` && &3 && ` ` && &4 && ` ` && &5.
        <entity>-new_aprov = 'Новый согласующий:' && ` ` && &6.
      ENDIF.

    END-OF-DEFINITION.


*    DEFINE m_add_output.
*      IF <req>-approver IS INITIAL AND
**         ( <req>-aenam EQ l_top_uname OR  " ins NaumovSM 16.12.20
**         <req>-aenam EQ lv_top_deleg ).  " ins NaumovSM 16.12.20
*         ( l_aenam EQ l_top_uname OR  " ins NaumovSM 16.12.20
*           l_aenam EQ lv_top_deleg ).  " ins NaumovSM 16.12.20
**        m_add_set <req> 'На согласовании' <req>-aenam TEXT-top c_11.  " ins NaumovSM 16.12.20
*        m_add_set <req> 'На согласовании' l_aenam TEXT-top c_11.  " ins NaumovSM 16.12.20
*      ELSE.  " ins NaumovSM 16.12.20
*        IF lv_actual_approver IS NOT INITIAL.
*          IF <req>-uname = lv_actual_approver.
*            lv_text_add_set = CONV string( TEXT-rab ).
*          ELSE.
*            lv_text_add_set = CONV string( TEXT-ruk ).
*          ENDIF.
*          m_add_set <req> 'На согласовании' lv_actual_approver lv_text_add_set c_11.
*        ELSE.
*          m_add_set <req> 'На согласовании' <req>-approver TEXT-ruk c_11.
*        ENDIF.
*      ENDIF.  " ins NaumovSM 16.12.20
*    END-OF-DEFINITION.

** Получаем полный актуальный маршрут согласования
*    get_appr_route( EXPORTING
*                      im_author = im_uname
*                    CHANGING
*                      ch_tab = lt_route ).
*    IF lt_route IS NOT INITIAL.    " ins NaumovSM
*      READ TABLE lt_route WITH KEY top = abap_true ASSIGNING FIELD-SYMBOL(<route>).
*      IF sy-subrc EQ 0.
** <-- ins NaumovSM 14.12.20 7700113464
*        IF <route>-orgeh IN zcl_tvarvc=>read( i_name = 'ZHR_FIO_GRA_TOP' i_type = 'S' ).   " <-- ins NaumovSM 14.12.20 7700113464
*          l_top_uname = lt_route[ ( lines( lt_route ) - 1 ) ]-uname.
*          DELETE lt_route INDEX ( lines( lt_route ) ).
*          DATA(lv_dop_top) = abap_true.
*        ELSE.
** --> ins NaumovSM 14.12.20 7700113464
*          l_top_uname = <route>-uname.
** <-- ins NaumovSM 14.12.20 7700113464
*        ENDIF.
*      ELSE.
*        l_top_uname = lt_route[ lines( lt_route ) ]-uname.
*        lv_dop_top = abap_true.
** --> ins NaumovSM 14.12.20 7700113464
*      ENDIF.
*    ENDIF.   " ins NaumovSM
*
** <-- ins NaumovSM 14.12.20 7700113464
**    get_deleg_from_tab( EXPORTING im_uname = l_top_uname
**                         CHANGING ch_uname_tab = lt_top_deleg ).
**    LOOP AT lt_top_deleg ASSIGNING FIELD-SYMBOL(<ls_top_deleg>).
**      lv_top_deleg = <ls_top_deleg>.
**      EXIT.
**    ENDLOOP.
*    lv_top_deleg = zcl_zhr_fio_util=>get_rep_from_hrus_d2( iv_uname = l_top_uname
*                                                           iv_reppr = zcl_zhr_fio_util=>c_reppr-zprem
*                                                           iv_date  = sy-datum ).
** --> ins NaumovSM 14.12.20 7700113464
*
*    SELECT SINGLE *
*             FROM zhrt_ui_kpi_gra
*             INTO CORRESPONDING FIELDS OF ls_request
*            WHERE reqid = im_reqid AND uname = im_uname.
*    CHECK sy-subrc = 0.
*
*    SELECT * FROM zhrt_ui_leave_st INTO TABLE lt_status.
*
*    CALL FUNCTION 'CHANGEDOCUMENT_READ'
*      EXPORTING
*        objectclass = c_obj_class "'ZHRT_UI_KPI_GRA'"
*        objectid    = CONV cdobjectv( im_reqid )
*      TABLES
*        editpos     = lt_history
*      EXCEPTIONS
*        OTHERS      = 1.
*
*    SORT lt_history BY udate DESCENDING utime DESCENDING.
*    APPEND ls_request TO lt_request.
*
*    DATA(lr_excl_history) = zcl_tvarvc=>read( i_name = 'ZHR_UI_KPI_EXCL' i_type = 'S' ).
*    DATA: lr_excl_changenr TYPE RANGE OF cdchangenr.
*    LOOP AT lr_excl_history ASSIGNING FIELD-SYMBOL(<ls_excl_history>).
*      APPEND INITIAL LINE TO lr_excl_changenr ASSIGNING FIELD-SYMBOL(<ls_excl_changenr>).
*      <ls_excl_changenr> = 'IEQ'.
*      <ls_excl_changenr>-low = CONV #( <ls_excl_history>-low ).
*    ENDLOOP.
*    IF lr_excl_changenr IS INITIAL.
*      APPEND INITIAL LINE TO lr_excl_changenr ASSIGNING <ls_excl_changenr>.
*      <ls_excl_changenr> = 'IEQ'.
*    ENDIF.
*
*    LOOP AT lt_history ASSIGNING FIELD-SYMBOL(<ls_history>) GROUP BY ( key1 = <ls_history>-udate
*                                                                       key2 = <ls_history>-utime ).
*
*      CHECK <ls_history>-changenr NOT IN lr_excl_changenr.  " ins NaumovSM
*
*      LOOP AT GROUP <ls_history> ASSIGNING FIELD-SYMBOL(<ls_member>).
*
*        ASSIGN COMPONENT <ls_member>-fname OF STRUCTURE ls_request TO FIELD-SYMBOL(<lv_old>).
*        IF sy-subrc = 0.
*          <lv_old> = <ls_member>-f_old.
*        ENDIF.
*
*      ENDLOOP.
*      APPEND ls_request TO lt_request.
*    ENDLOOP.
*    SORT lt_request BY rdate ASCENDING rtime ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM lt_request COMPARING rdate rtime.
*
** По истории собираем историчесткий маршрут
*    LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<req>).
*      READ TABLE lt_history_route WITH KEY uname = <req>-approver TRANSPORTING NO FIELDS.
*      IF sy-subrc NE 0.
*        APPEND VALUE #( uname = <req>-approver ) TO lt_history_route.
*      ENDIF.
*    ENDLOOP.
*
** Заполняем комментарий отклонения
*    LOOP AT lt_request ASSIGNING <req>.
*      CLEAR l_text.
*      l_textname = |{ <req>-reqid }D{ <req>-rdate }{ <req>-rtime }|.
*      CALL METHOD read_text(
*        IMPORTING
*          text_id = l_textname
*        CHANGING
*          text    = l_text ).
*      <req>-declinecommentary = l_text.
*    ENDLOOP.


    CALL METHOD zcl_zhr_kpi_app_dpc_ext=>get_history
      EXPORTING
        iv_reqid         = im_reqid
        iv_uname         = im_uname
        iv_del           = 'X'
      IMPORTING
        et_route         = lt_route
        et_history_route = lt_history_route
        et_request       = lt_request
        ev_top_deleg     = lv_top_deleg
        ev_top_uname     = l_top_uname
        ev_dop_top       = lv_dop_top
        es_ui_kpi_gra    = ls_request.

* Получаем полный актуальный маршрут согласования
    get_appr_route_all( EXPORTING
                         im_author = im_uname
                        CHANGING
                         ch_tab    = lt_route_main_all ).
    DATA(lt_request_copy) = lt_request[].
    READ TABLE lt_route WITH KEY zam = abap_true ASSIGNING FIELD-SYMBOL(<route>).
    IF sy-subrc EQ 0.
      l_zam_uname = <route>-uname.
    ENDIF.
* ТЕКСТЫ СТАТУСОВ ПРОПИСАНЫ В КОДЕ ДЛЯ УДОБСТВА ЧТЕНИЯ КОДА

* Добавляем первый блок
    READ TABLE lt_request ASSIGNING FIELD-SYMBOL(<req>) INDEX 1.
    CHECK sy-subrc EQ 0.
    READ TABLE lt_request ASSIGNING FIELD-SYMBOL(<req2>) WITH KEY status = c_st_02.
    IF sy-subrc NE 0.
      m_add_set <req> 'В работе' <req>-uname TEXT-rab c_01.
    ELSE.
      m_add_set <req> 'Создал(а) отчёт' <req>-uname TEXT-rab c_02.
    ENDIF.
*{ Иванов А.А. 7700161044 Актуализация маршрута согласования
*    IF lines( lt_request ) > 1.
*      ls_prew_req = lt_request[ lines( lt_request ) - 1 ].
*    ENDIF.
*} Иванов А.А. 7700161044 Актуализация маршрута согласования
    IF <req>-status EQ c_st_01.
*      DELETE lt_request INDEX 1.
      LOOP AT lt_request ASSIGNING <req2>.
        IF <req2>-status EQ c_st_01.
          DELETE TABLE lt_request FROM <req2>.
          CONTINUE.
        ELSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
    UNASSIGN <req>.

** Заполнение таймлана
*    LOOP AT lt_history_route ASSIGNING FIELD-SYMBOL(<ls_history_route_idx>).
*      LOOP AT lt_route ASSIGNING  FIELD-SYMBOL(<ls_route_idx>) WHERE uname = <ls_history_route_idx>-uname
*                                                                 AND add_fl IS INITIAL.
*        <ls_history_route_idx>-idx = <ls_route_idx>-idx.
*      ENDLOOP.
*    ENDLOOP.
*    LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_request_idx>).
*      LOOP AT lt_route ASSIGNING <ls_route_idx> WHERE ( uname = <ls_request_idx>-aenam
*                                                   OR uname = <ls_request_idx>-deleg )
*                                                  AND add_fl IS INITIAL.
*        <ls_request_idx>-idx = <ls_route_idx>-idx.
*      ENDLOOP.
*    ENDLOOP.
*
*    DATA lv_previous_idx TYPE zhr_ui_kpi_gra_tt-idx.
*    LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_route_decline>) WHERE idx IS INITIAL.
*      LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_route_previous>) WHERE approver = <ls_route_decline>-aenam
*                                                                       AND ( aenam = <ls_route_decline>-approver
*                                                                          OR deleg = <ls_route_decline>-approver )
*                                                                       AND idx <> lv_previous_idx.
*        <ls_route_decline>-idx = <ls_route_previous>-idx.
*        lv_previous_idx = <ls_route_previous>-idx.
*      ENDLOOP.
*    ENDLOOP.
*    IF sy-subrc = 0.
*
*    ENDIF.

    LOOP AT lt_request ASSIGNING <req>.
      CHECK sy-tabix NE 1.
      DATA(lv_cur_txt) = COND string( WHEN <req>-aenam = l_zam_uname THEN TEXT-zam ELSE TEXT-ruk ).
      l_idx = sy-tabix + 1.

* <-- ins NaumovSM 16.12.20
      CLEAR: lv_appr_deleg.
      REFRESH: lt_appr_deleg.
*      IF <req>-approver IS NOT INITIAL.   " del NaumovSM
      IF <req>-approver IS NOT INITIAL AND l_status <> '04'.   " ins NaumovSM
        lv_appr_deleg = zcl_zhr_fio_util=>get_rep_from_hrus_d2( iv_uname = <req>-approver
                                                                iv_reppr = zcl_zhr_fio_util=>c_reppr-zprem
                                                                iv_date  = sy-datum ).
*        get_deleg_from_tab( EXPORTING im_uname = <req>-approver
*                             CHANGING ch_uname_tab = lt_appr_deleg ).
*
*        LOOP AT lt_appr_deleg ASSIGNING FIELD-SYMBOL(<ls_appr_deleg>).
*          lv_appr_deleg = <ls_appr_deleg>.
**          <req>-approver = <ls_appr_deleg>.
*          EXIT.
*        ENDLOOP.
      ENDIF.
*
*      CLEAR: lv_appr_deleg.
*      REFRESH: lt_appr_deleg.
*      IF <req>-aenam IS NOT INITIAL.
*
*        get_deleg_from_tab( EXPORTING im_uname = <req>-aenam
*                             CHANGING ch_uname_tab = lt_appr_deleg ).
*
*        LOOP AT lt_appr_deleg ASSIGNING FIELD-SYMBOL(<ls_aenam_deleg>).
**          lv_appr_deleg = <ls_appr_deleg>.
*          <req>-aenam = <ls_aenam_deleg>.
*          EXIT.
*        ENDLOOP.
*      ENDIF.
* --> ins NaumovSM 16.12.20
      CASE <req>-status.
        WHEN c_st_01.
          READ TABLE lt_request ASSIGNING <req2> INDEX l_idx.
          IF sy-subrc EQ 0.
            IF <req2>-status NE c_st_01.
              m_add_set <req> 'Доработал(а) отчёт' <req>-approver TEXT-rab c_03.
            ENDIF.
          ELSE.
            m_add_set <req> 'Доработка' <req>-approver TEXT-rab c_04.
          ENDIF.

        WHEN c_st_02.
          IF <req>-approver EQ <req>-aenam AND l_appr IS INITIAL AND <req>-deleg IS INITIAL.
            CONTINUE.
          ENDIF.
*         Анализируем актуальный маршрут
          CLEAR: l_idx_appr1, l_idx_appr2.
*          READ TABLE lt_route WITH KEY uname = <req>-aenam TRANSPORTING NO FIELDS.
*          IF sy-subrc EQ 0.
*            l_idx_appr1 = sy-tabix.
* <-- NaumovSM 14.12.20
*          DATA(lv_change_aenam) = COND uname( WHEN <req>-deleg IS NOT INITIAL
*                                              THEN <req>-deleg
*                                              ELSE <req>-aenam ).
*          READ TABLE lt_route WITH KEY uname = <req>-aenam TRANSPORTING NO FIELDS.
*          READ TABLE lt_route WITH KEY uname = lv_change_aenam TRANSPORTING NO FIELDS. " del NaumovSM

          IF <req>-idx IS NOT INITIAL.
            READ TABLE lt_route WITH KEY idx = <req>-idx TRANSPORTING NO FIELDS.
          ELSE.
*            READ TABLE lt_route WITH KEY uname = lv_change_aenam TRANSPORTING NO FIELDS.
            READ TABLE lt_route WITH KEY uname = <req>-aenam TRANSPORTING NO FIELDS.
          ENDIF.

          IF sy-subrc EQ 0.
            l_idx_appr1 = sy-tabix.

            READ TABLE lt_request ASSIGNING FIELD-SYMBOL(<ls_request_next>) INDEX l_idx.
            IF sy-subrc = 0.
              lv_idx_next = <ls_request_next>-idx + 1.
              IF lines( lt_route ) > 1.   " ins NaumovSM 30.04.2021
                READ TABLE lt_route WITH KEY idx = <ls_request_next>-idx ASSIGNING FIELD-SYMBOL(<ls_next_route>).
              ELSE.   " ins NaumovSM 30.04.2021
                l_idx_appr2 = <ls_request_next>-idx + 1.   " ins NaumovSM 30.04.2021
              ENDIF.   " ins NaumovSM 30.04.2021
            ELSE.
              IF <req>-declinecommentary IS NOT INITIAL.
                DATA(lv_prev_idx) = <req>-idx - 1.
                READ TABLE lt_route WITH KEY idx = lv_prev_idx ASSIGNING <ls_next_route>.
                lv_idx_next = <req>-idx.
              ELSE.
                lv_idx_next = <req>-idx + 1.
                READ TABLE lt_route WITH KEY idx = lv_idx_next ASSIGNING <ls_next_route>.
                IF sy-subrc <> 0.   " ищем предыдущую запись маршрута
                  lv_idx_next = lv_idx_next - 2.
                  READ TABLE lt_route WITH KEY idx = lv_idx_next ASSIGNING <ls_next_route>.
***                READ TABLE lt_route WITH KEY idx = <req>-idx ASSIGNING <ls_next_route>.
                ENDIF.
              ENDIF.
            ENDIF.

*            READ TABLE lt_route WITH KEY uname = <req>-approver TRANSPORTING NO FIELDS.
            IF sy-subrc EQ 0 AND l_idx_appr2 IS INITIAL.
              l_idx_appr2 = sy-tabix.
            ENDIF.

            IF ls_request-aenam = <req>-aenam OR    " del NaumovSM
               ls_request-aenam = <req>-deleg.   " ins NaumovSM
*              READ TABLE lt_route WITH KEY uname = <req>-aenam   " ins NaumovSM
              IF ls_request-aenam = <req>-aenam.
                READ TABLE lt_route WITH KEY uname = <req>-aenam   " del NaumovSM
                                               add_fl = abap_true
                                  TRANSPORTING NO FIELDS.
              ELSE.
                READ TABLE lt_route WITH KEY uname = <req>-deleg   " del NaumovSM
                                               add_fl = abap_true
                                  TRANSPORTING NO FIELDS.
              ENDIF.
*              READ TABLE lt_route WITH KEY uname = lv_change_aenam   " del NaumovSM
*                                             add_fl = abap_true
*                                TRANSPORTING NO FIELDS.
              IF sy-subrc EQ 0.
                l_idx_appr1 = sy-tabix.
              ENDIF.
            ENDIF.

            IF ls_request-aenam = <req>-approver.
              READ TABLE lt_route WITH KEY uname  = <req>-approver
                                           add_fl = abap_true
                               ASSIGNING <ls_next_route>.
              IF sy-subrc = 0.
                l_idx_appr2 = sy-tabix.
              ENDIF.
            ENDIF.

            IF <req>-approver IS INITIAL.
              l_idx_appr2 = l_idx_appr1.
            ENDIF.
* { старая логика - ошибки из-за неправильного определения в lt_route
*********          READ TABLE lt_route WITH KEY uname = <req>-aenam TRANSPORTING NO FIELDS.
*********          IF sy-subrc EQ 0.
*********            l_idx_appr1 = sy-tabix.
*********            READ TABLE lt_route WITH KEY uname = <req>-approver TRANSPORTING NO FIELDS.
*********            IF sy-subrc EQ 0.
*********              l_idx_appr2 = sy-tabix.
*********            ENDIF.
*********
*********            IF ls_request-aenam = <req>-aenam.
*********              READ TABLE lt_route WITH KEY uname = <req>-aenam
*********                                             add_fl = abap_true
*********                                TRANSPORTING NO FIELDS.
*********              IF sy-subrc EQ 0.
*********                l_idx_appr1 = sy-tabix.
*********              ENDIF.
*********            ENDIF.
*********
*********            IF ls_request-aenam = <req>-approver.
*********              READ TABLE lt_route WITH KEY uname  = <req>-approver
*********                                           add_fl = abap_true
*********                              TRANSPORTING NO FIELDS.
*********              IF sy-subrc = 0.
*********                l_idx_appr2 = sy-tabix.
*********              ENDIF.
*********            ENDIF.
*********
*********            IF <req>-approver IS INITIAL.
*********              l_idx_appr2 = l_idx_appr1.
*********            ENDIF.
* } старая логика - ошибки из-за неправильного определения в lt_route
* --> NaumovSM 14.12.20
          ENDIF.

*         Если в актуальном не нашли пользователей, то анализируем исторический
          IF l_idx_appr1 IS INITIAL OR l_idx_appr2 IS INITIAL.
            CLEAR: l_idx_appr1, l_idx_appr2.
**            READ TABLE lt_history_route WITH KEY uname = <req>-aenam TRANSPORTING NO FIELDS.
*            READ TABLE lt_history_route WITH KEY uname = lv_change_aenam TRANSPORTING NO FIELDS.
            IF <req>-deleg IS INITIAL.
              READ TABLE lt_history_route WITH KEY uname = <req>-aenam TRANSPORTING NO FIELDS.
            ELSE.
              READ TABLE lt_history_route WITH KEY uname = <req>-deleg TRANSPORTING NO FIELDS.
            ENDIF.
            IF sy-subrc EQ 0.
              l_idx_appr1 = sy-tabix.
              READ TABLE lt_history_route WITH KEY uname = <req>-approver  ASSIGNING <ls_next_route>.
              IF sy-subrc EQ 0.
                l_idx_appr2 = sy-tabix.
              ENDIF.
            ENDIF.
          ENDIF.

*         согласование вверх
          IF l_idx_appr1 < l_idx_appr2.
            IF <req>-aenam NE <req>-uname.
*            IF  lv_change_aenam NE <req>-uname.
              IF <req>-approver EQ l_aenam AND
                 <ls_next_route>-add_fl IS INITIAL.
                m_add_set <req> 'Скорректировал(а) отчёт' <req>-aenam lv_cur_txt c_05. "2003
*                m_add_set <req> 'Скорректировал(а) отчёт' <req>-aenam TEXT-ruk c_05.
*                m_add_set <req> 'Скорректировал(а) отчёт'  lv_change_aenam TEXT-ruk c_05.
              ELSE.
                m_add_set <req> 'Согласовал(а) отчёт' <req>-aenam lv_cur_txt c_06. "2003
*                m_add_set <req> 'Согласовал(а) отчёт' <req>-aenam TEXT-ruk c_06.
*                m_add_set <req> 'Согласовал(а) отчёт'  lv_change_aenam TEXT-ruk c_06.
              ENDIF.
            ELSE.
* <-- ins NaumovSM 15.12.2020
              IF ls_request-aenam = <req>-aenam.
*              IF ls_request-aenam =  lv_change_aenam.
                READ TABLE lt_route WITH KEY uname = <req>-aenam
*                READ TABLE lt_route WITH KEY uname =  lv_change_aenam
                                            add_fl = abap_true
                                  TRANSPORTING NO FIELDS.
                IF sy-subrc EQ 0.
                  m_add_set <req> 'Согласовал(а) отчёт' <req>-aenam lv_cur_txt c_06. "2003
*                  m_add_set <req> 'Согласовал(а) отчёт'  lv_change_aenam TEXT-ruk c_06.
                ENDIF.
              ELSE.
* --> ins NaumovSM 15.12.2020
*                m_add_set <req> 'Доработал(а) отчёт' <req>-aenam TEXT-ruk c_03.   " <-- del NaumovSM 15.12.2020
                m_add_set <req> 'Доработал(а) отчёт' <req>-aenam TEXT-rab c_03.   " <-- ins NaumovSM 15.12.2020
*                m_add_set <req> 'Доработал(а) отчёт'  lv_change_aenam TEXT-rab c_03.   " <-- ins NaumovSM 15.12.2020
              ENDIF.
            ENDIF.
          ENDIF.

*         согласование вниз - отклонение или возврат
          IF l_idx_appr1 > l_idx_appr2.
            m_add_set <req> 'Вернул(а) на доработку' <req>-aenam lv_cur_txt c_07. "2003
*            m_add_set <req> 'Вернул(а) на доработку'  lv_change_aenam TEXT-ruk c_07.
          ENDIF.

          IF l_idx_appr1 = l_idx_appr2.
            IF l_status <> '04'.
              IF ( <req>-approver NE l_appr AND
                   <req>-approver NE lv_appr_deleg )
                AND <req>-status < l_status.
                m_add_set <req> 'Отозвал(а) решение' <req>-aenam lv_cur_txt c_08. "2003
*                m_add_set <req> 'Отозвал(а) решение'  lv_change_aenam TEXT-ruk c_08.
              ELSE.
                IF <req2>-status EQ c_st_02.
*                  m_add_set <req> 'Доработал(а) отчёт' <req>-aenam TEXT-ruk c_03.
                ELSE.
                  m_add_set <req> 'Согласовал(а) отчёт' <req>-aenam lv_cur_txt c_06. "2003
*                  m_add_set <req> 'Согласовал(а) отчёт'  lv_change_aenam TEXT-ruk c_06.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
          IF <entity> IS NOT ASSIGNED.
            CONTINUE.
          ENDIF.

        WHEN c_st_03.

          IF lv_dop_top = abap_true.  " ins NaumovSM 16.12.20
            lv_role = TEXT-ruk.  " ins NaumovSM 16.12.20
          ELSE.  " ins NaumovSM 16.12.20
            lv_role = TEXT-top.  " ins NaumovSM 16.12.20
          ENDIF.  " ins NaumovSM 16.12.20
          IF <req>-aenam = l_zam_uname. "2003
            lv_role = TEXT-zam.
          ENDIF.


*          IF <req>-aenam EQ l_top_uname.  " del NaumovSM 16.12.20
          IF <req>-aenam EQ l_top_uname OR  " ins NaumovSM 16.12.20
             <req>-aenam EQ lv_top_deleg.  " ins NaumovSM 16.12.20

*            IF <req>-aenam = l_aenam. " del NaumovSM
            IF <req>-aenam = l_aenam AND <req>-approver IS NOT INITIAL .  " ins NaumovSM
              m_add_set <req> 'Отозвал(а) решение' <req>-aenam lv_role c_08.
            ELSE.
              m_add_set <req> 'Согласовано' <req>-aenam lv_role c_09.
            ENDIF.

          ELSE.

* <-- NaumovSM 29.12.20 Заглушка на случай когда связь с топом потеряна
            IF check_top_man( im_uname = <req>-aenam
                              im_date  = <req>-rdate ) = abap_true.   " ins NaumovSM 02.02.2021
              m_add_set <req> 'Согласовано' <req>-aenam lv_role c_09.
            ELSE.
              IF l_status <> c_st_02.
                m_add_set <req> 'Отозвал(а) решение' <req>-aenam TEXT-adm c_08.
              ELSE.
                m_add_set <req> 'Согласовано' <req>-aenam lv_role c_09.
              ENDIF.
            ENDIF.
* --> NaumovSM 29.12.20

          ENDIF.

        WHEN c_st_04.
          m_add_set <req> 'Вернул(а) на доработку' <req>-aenam lv_cur_txt c_07. "2003
          READ TABLE lt_request ASSIGNING <req2> INDEX l_idx.
          IF sy-subrc EQ 0.
            IF <req2>-status EQ c_st_01.
              m_add_set <req> 'Доработка' <req>-approver TEXT-rab c_04.
            ELSEIF <req2>-status EQ c_st_02.
              m_add_set <req> 'Доработал(а) отчёт' <req>-approver TEXT-rab c_03.
            ENDIF.
          ELSE.
            m_add_set <req> 'Доработка' <req>-approver TEXT-rab c_04.
          ENDIF.

        WHEN c_st_06.
          m_add_set <req> 'Принял(а) отчёт' <req>-aenam TEXT-adm c_10.
        WHEN OTHERS.

      ENDCASE.

      UNASSIGN <entity>.

*      IF lv_appr_deleg IS NOT INITIAL.   " ins NaumovSM 16.12.20
*        l_appr = lv_appr_deleg.   " ins NaumovSM 16.12.20
*      ELSE.   " ins NaumovSM 16.12.20
      l_appr = <req>-approver.
*      ENDIF.   " ins NaumovSM 16.12.20
      l_aenam = <req>-aenam.
*      l_aenam = COND #( WHEN lv_change_aenam IS NOT INITIAL
*                        THEN lv_change_aenam
*                        ELSE <req>-aenam ).
      l_status = <req>-status.
    ENDLOOP.

* Добавляем последний блок, если он нужен
    IF <req> IS ASSIGNED.
* { ins NaumovSM
* если актуальная запись отклонена ищем предыдущую неотклонённую запись по данному маршруту
      IF <req>-declinecommentary IS NOT INITIAL.
        DO lines( lt_request ) TIMES.
          DATA(lv_actual_idx) = lines( lt_request ) - sy-index + 1.
* считываем предыдущую не отклонённую запись с предыдущим индексом маршрута
          LOOP AT lt_request FROM lv_actual_idx ASSIGNING FIELD-SYMBOL(<ls_actual_req>) WHERE declinecommentary IS INITIAL
                                                                  AND idx = ( <req>-idx - 1 ).
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            DATA(lv_actual_approver) = <ls_actual_req>-aenam.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
* } ins NaumovSM.
      IF <req>-status EQ c_st_02.
        IF <req>-approver IS INITIAL AND
*           ( <req>-aenam EQ l_top_uname OR  " ins NaumovSM 16.12.20
*           <req>-aenam EQ lv_top_deleg ).  " ins NaumovSM 16.12.20
           ( l_aenam EQ l_top_uname OR  " ins NaumovSM 16.12.20
             l_aenam EQ lv_top_deleg ).  " ins NaumovSM 16.12.20
*          m_add_set <req> 'На согласовании' <req>-aenam TEXT-top c_11.  " ins NaumovSM 16.12.20
          IF l_aenam = l_zam_uname.
            m_add_set <req> 'На согласовании' l_aenam TEXT-zam c_11.
          ELSE.
            m_add_set <req> 'На согласовании' l_aenam TEXT-top c_11.  " ins NaumovSM 16.12.20
          ENDIF.
        ELSE.  " ins NaumovSM 16.12.20
          IF lv_actual_approver IS NOT INITIAL.
            IF <req>-uname = lv_actual_approver.
              lv_text_add_set = CONV string( TEXT-rab ).
            ELSE.
              lv_text_add_set = CONV string( TEXT-ruk ).
            ENDIF.
            IF <req>-uname = l_zam_uname.
              m_add_set <req> 'На согласовании' lv_actual_approver TEXT-zam c_11.
            ELSE.
              m_add_set <req> 'На согласовании' lv_actual_approver lv_text_add_set c_11.
            ENDIF.
          ELSE.
            IF <req>-approver = l_zam_uname.
              m_add_set <req> 'На согласовании' <req>-approver TEXT-zam c_11.
            ELSE.
              m_add_set <req> 'На согласовании' <req>-approver TEXT-ruk c_11.
            ENDIF.
          ENDIF.
        ENDIF.  " ins NaumovSM 16.12.20
      ENDIF.
    ENDIF.

    DELETE ADJACENT DUPLICATES FROM ch_set COMPARING status_text username deleg commentary.
* Редактируем записи топа, т.к. при согласовании у него операции такие же как у руководителя
*    LOOP AT ch_set ASSIGNING FIELD-SYMBOL(<line>) WHERE username = l_top_uname.   " del NaumovSM 16.12.20
    LOOP AT ch_set ASSIGNING FIELD-SYMBOL(<line>) WHERE username EQ lv_top_deleg   " ins NaumovSM 16.12.20
                                                     OR username EQ l_top_uname.   " ins NaumovSM 16.12.20
*{ Иванов А.А. 7700161044 Актуализация маршрута согласования
*      CHECK <line>-username IS NOT INITIAL.
*} Иванов А.А. 7700161044 Актуализация маршрута согласования
      IF lv_dop_top = abap_true .  " ins NaumovSM 16.12.20.
        <line>-role = TEXT-ruk.
      ELSE.
        <line>-role = TEXT-top.
      ENDIF.
    ENDLOOP.

* Заполняем ФИО и наименование должности
    LOOP AT ch_set ASSIGNING <line> GROUP BY <line>-username.
*{ Иванов А.А. 7700161044 Актуализация маршрута согласования
*      CHECK <line>-username IS NOT INITIAL.
*} Иванов А.А. 7700161044 Актуализация маршрута согласования
      CLEAR: pernr, _bipernr, _biplans.
      CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
        EXPORTING
          user_name               = CONV uname( <line>-username )
        IMPORTING
          employeenumber          = pernr
        EXCEPTIONS
          no_employeenumber_found = 1
          subtype_not_available   = 2
          OTHERS                  = 3.
      CHECK pernr IS NOT INITIAL.

*      m_get_objec c_otype_p pernr _bipernr.   " del NaumovSM
      m_get_objec c_otype_p pernr _bipernr sy-datum.   " ins NaumovSM
      IF im_inits_fl EQ abap_true.
*        m_read_inf pernr '0002' lt_p0002.
        m_read_inf pernr '0002' lt_p0002 sy-datum.
        IF lt_p0002[] IS NOT INITIAL.
          <line>-fio = |{ lt_p0002[ 1 ]-nachn } { lt_p0002[ 1 ]-vorna+0(1) }. { lt_p0002[ 1 ]-midnm+0(1) }.|.
        ENDIF.
      ELSE.
        IF _bipernr IS NOT INITIAL.
          <line>-fio = _bipernr->get_name( adatum = sy-datum anoauth = abap_true ).
        ENDIF.
      ENDIF.
      TRY.
          _bipernr->get_orgass(
            EXPORTING
              adatum = sy-datum
            IMPORTING
              resplans = _biplans
        ).
*         Наименование должности
          IF _biplans IS NOT INITIAL.
            <line>-plnam = _biplans->get_name( adatum = sy-datum anoauth = abap_true ).
            TRANSLATE <line>-plnam+0(1) TO UPPER CASE.
          ENDIF.
        CATCH cx_root.
      ENDTRY.
      LOOP AT GROUP <line> ASSIGNING FIELD-SYMBOL(<line_2>).
        <line_2>-fio = <line>-fio.
        <line_2>-plnam = <line>-plnam.
        <line_2>-fio_d = zcl_zhr_fio_util=>get_uname_fio( im_uname = <line_2>-deleg im_inits_fl = 'X' ).   " ins NaumovSM 10.03.2021
      ENDLOOP.

    ENDLOOP.
* Заполняем ИД чтобы не падал сет по ключу
    LOOP AT ch_set ASSIGNING <line>.
      <line>-id = sy-tabix.
*     Чистим комментарий, если статус не отклонение оценки
      CHECK <line>-status NE c_07.
      CLEAR <line>-commentary.
    ENDLOOP.
    IF io_tech_request_context IS NOT INITIAL.
      DATA ls_paging TYPE /iwbep/s_mgw_paging.
      ls_paging-top  = io_tech_request_context->get_top( ).
      ls_paging-skip = io_tech_request_context->get_skip( ).

      /iwbep/cl_mgw_data_util=>paging(
        EXPORTING
          is_paging = ls_paging
        CHANGING
          ct_data   = ch_set ).
    ENDIF.

*{ Иванов А.А. 7700161044 Актуализация маршрута согласования

    DATA(lv_tabix) = 1.
    LOOP AT ch_set ASSIGNING <line> WHERE status = c_st_06 OR status = c_st_11
      OR status = c_09 OR status = c_07. " Тихонов Г.Ю.
      CHECK sy-tabix > 1.
      DATA(lv_ind) =  COND #( WHEN lines( lt_request_copy ) = 1 THEN lv_tabix ELSE lv_tabix + 1 ).
*      IF <line>-status = c_st_06.
*        LOOP AT lt_request_copy FROM lv_ind ASSIGNING <req> WHERE aenam = <line>-username  AND status = c_st_02.
*          lv_tabix = sy-tabix.
*          EXIT.
*        ENDLOOP.
*      ELSE.
*         LOOP AT lt_request_copy FROM lv_ind ASSIGNING <req> WHERE ( aenam = sy-uname OR aenam = <line>-username OR approver = sy-uname ) AND
**{ Тихонов Г.Ю.
**           status = c_st_02.
*           ( status = c_st_02 or status = c_st_03 ).
**} Тихонов Г.Ю.
*          lv_tabix = sy-tabix.
*          EXIT.
*        ENDLOOP.
*      ENDIF.
      LOOP AT lt_request_copy FROM lv_ind ASSIGNING <req> WHERE approver = <line>-username AND
         ( status = c_st_02 or status = c_st_03 ).
        lv_tabix = sy-tabix.
        EXIT.
      ENDLOOP.
      CHECK lv_tabix > 1 OR lines( lt_request_copy ) = 1 .
      ls_prew_req = COND #( WHEN lines( lt_request_copy ) = 1 THEN lt_request_copy[ lv_tabix ] ELSE lt_request_copy[ lv_tabix - 1 ] ).
      ls_prew_req-rtime = <req>-rtime.
      CLEAR lv_tabix.
      CHECK <req>-approver IS NOT INITIAL.
      READ TABLE lt_route_main_all INTO ls_route_main_all WITH KEY uname = <req>-approver.
      IF sy-subrc = 0.
        DATA(lv_cur_tabix) = sy-tabix.
        LOOP AT lt_route_main_all INTO ls_route_main_all WHERE uname     IS INITIAL AND (
                                                               vacancy   IS NOT INITIAL OR
                                                               without_s IS NOT INITIAL ).
          CHECK sy-tabix <= lv_cur_tabix.
          IF lines( lt_request_copy ) = 1 AND lv_req IS INITIAL.
           lv_req = <line>.
           DELETE ch_set.
          ENDIF.
*          m_get_approver_role <trip>-approver l_txt_route.
          CASE abap_true.
            WHEN ls_route_main_all-ssp_boss.
              l_txt_route = TEXT-ruv.
            WHEN ls_route_main_all-no_ses.
              l_txt_route = TEXT-ruv.
            WHEN ls_route_main_all-nep_boss.
              l_txt_route = TEXT-ruv.
            WHEN ls_route_main_all-top.
              l_txt_route = TEXT-tov.
            WHEN OTHERS.
          ENDCASE.
          CASE abap_true.
            WHEN ls_route_main_all-vacancy.
              CLEAR ls_prew_req-declinecommentary.
              m_ins_set ls_prew_req 'На согласовании' '' l_txt_route ''.
*              READ TABLE lt_route_main_all WITH KEY without_s = abap_true TRANSPORTING NO FIELDS.
*              IF sy-subrc <> 0 OR sy-tabix > lv_cur_tabix .
*                m_add_output.
*              ENDIF.
            WHEN ls_route_main_all-without_s.
              m_add_set_witout_s ls_prew_req 'Произведена автоматическая замена согласующего руководителя на основании изменения орг. структуры' '' '' '' ''.
              m_read_inf ls_route_main_all-pernr '0002' lt_p0002_ws sy-datum.
              READ TABLE lt_p0002_ws INDEX 1 INTO DATA(ls_p0002_ws).
              CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
                EXPORTING
                  user_name               = CONV uname( <req>-approver )
                IMPORTING
                  employeenumber          = l_pernr
                EXCEPTIONS
                  no_employeenumber_found = 1
                  subtype_not_available   = 2
                  OTHERS                  = 3.

              CHECK l_pernr IS NOT INITIAL.
              m_read_inf l_pernr '0002' lt_p0002 sy-datum.
              READ TABLE lt_p0002 INDEX 1 ASSIGNING FIELD-SYMBOL(<p0002>).
              IF sy-subrc EQ 0.
                DATA(lv_approver_fio) = |{ <p0002>-nachn } { <p0002>-vorna } { <p0002>-midnm }|.
              ENDIF.
              m_add_set_witout_s ls_prew_req '' ls_p0002_ws-nachn ls_p0002_ws-vorna ls_p0002_ws-midnm lv_approver_fio.
            WHEN OTHERS.
          ENDCASE.
*          DELETE lt_route_main_all.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    LOOP AT ch_set ASSIGNING <line> WHERE status = c_st_11.
      LOOP AT lt_request_copy ASSIGNING <req> WHERE approver = <line>-username AND prev_approver IS NOT INITIAL.
        m_add_set_witout_s <req> 'Произведена автоматическая замена согласующего руководителя на основании изменения орг. структуры' '' '' '' ''.
        CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
             EXPORTING
               user_name               = CONV uname( <req>-prev_approver )
             IMPORTING
               employeenumber          = l_pernr
             EXCEPTIONS
               no_employeenumber_found = 1
               subtype_not_available   = 2
               OTHERS                  = 3.

           CHECK l_pernr IS NOT INITIAL.
           m_read_inf l_pernr '0002' lt_p0002_ws sy-datum.
           READ TABLE lt_p0002_ws INDEX 1 INTO ls_p0002_ws.

           CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
             EXPORTING
               user_name               = CONV uname( <req>-approver )
             IMPORTING
               employeenumber          = l_pernr
             EXCEPTIONS
               no_employeenumber_found = 1
               subtype_not_available   = 2
               OTHERS                  = 3.

          CHECK l_pernr IS NOT INITIAL.
          m_read_inf l_pernr '0002' lt_p0002 sy-datum.
          READ TABLE lt_p0002 INDEX 1 ASSIGNING <p0002>.
          IF sy-subrc EQ 0.
            lv_approver_fio = |{ <p0002>-nachn } { <p0002>-vorna } { <p0002>-midnm }|.
          ENDIF.

          m_add_set_witout_s <req> '' ls_p0002_ws-nachn ls_p0002_ws-vorna ls_p0002_ws-midnm lv_approver_fio.
          CLEAR: l_pernr, lv_approver_fio, ls_p0002_ws.
      ENDLOOP.
    ENDLOOP.

    IF lv_req IS NOT INITIAL.
      APPEND lv_req TO ch_set.
    ENDIF.

*    SORT ch_set BY date time ASCENDING.
    LOOP AT ch_set ASSIGNING <line>.
      <line>-id = sy-tabix.
    ENDLOOP.

*    DELETE ADJACENT DUPLICATES FROM ch_set COMPARING username.

*} Иванов А.А. 7700161044 Актуализация маршрута согласования
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->GET_YEARS_SET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_UNAME                       TYPE        UNAME
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [--->] IM_INITS_FL                    TYPE        FLAG(optional)
* | [<-->] CH_SET                         TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_YEARS
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_years_set.

    DATA:
*        , lt_p0105     TYPE TABLE OF p0105
*        , lt_hire_fire TYPE TABLE OF phifi
*        , l_pernr      TYPE persno
          lv_year  TYPE gjahr VALUE '2019'   " начальный год по умолчанию
        .
*
*    CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
*      EXPORTING
*        user_name               = im_uname
*      IMPORTING
*        employeenumber          = l_pernr
*      EXCEPTIONS
*        no_employeenumber_found = 1
*        subtype_not_available   = 2
*        OTHERS                  = 3.
*
*    CHECK l_pernr IS NOT INITIAL.
*
    DATA(lv_curr_year) = CONV gjahr( sy-datum(4) ).
*
** Получаем года за которые отчёт существует
*    SELECT yeara
*      FROM zhrt_ui_kpi_gra AS gra
*      INTO TABLE @DATA(lt_years)
*     WHERE uname EQ @im_uname.
*
*    SORT lt_years.
*    DELETE ADJACENT DUPLICATES FROM lt_years.
*    LOOP AT lt_years ASSIGNING FIELD-SYMBOL(<ls_years>)
*                         WHERE yeara = lv_curr_year.
*      EXIT.
*    ENDLOOP.
*    IF sy-subrc <> 0.
*      lt_years = VALUE #( BASE lt_years ( yeara = lv_curr_year ) ).
*    ENDIF.
*
*    SORT lt_years.
*
*    LOOP AT lt_years ASSIGNING <ls_years>.
*      ch_set = VALUE #( BASE ch_set ( year = <ls_years>-yeara ) ).
*    ENDLOOP.

*    CALL FUNCTION 'HRF_HIRE_FIRE'
*      EXPORTING
*        pernr         = l_pernr
**       BEGDA         = '18000101'
**       ENDDA         = '99991231'
**       LANGUAGE      =
**       PERSON        =
*      TABLES
*        ms_data       = lt_hire_fire
*      EXCEPTIONS
*        reject_pernr  = 1
*        display_error = 2
*        OTHERS        = 3.

*    IF sy-subrc = 0.
*      LOOP AT lt_hire_fire ASSIGNING FIELD-SYMBOL(<ls_hire_fire>)
*                               WHERE stat2 = '3'.   " ищем активный статус
*        DATA(lv_year) = CONV gjahr( <ls_hire_fire>-begda(4) ).
*        EXIT.
*      ENDLOOP.
** Implement suitable error handling here
*    ENDIF.
*
*    IF lv_year IS NOT INITIAL.
*    m_read_inf l_pernr '0105' lt_p0105.
*    IF lt_p0105[] IS NOT INITIAL.
*      DATA(lv_curr_year) = CONV gjahr( sy-datum(4) ).
*      LOOP AT lt_p0105 ASSIGNING FIELD-SYMBOL(<ls_p0105>)
*                           WHERE subty = '0001'.
*        DATA(lv_year) = CONV gjahr( <ls_p0105>-begda(4) ).
*        EXIT.
*      ENDLOOP.
*      IF lv_curr_year <= lv_year.
*        ch_set = VALUE #( ( year = lv_year ) ).
*      ELSE.

    WHILE lv_curr_year >= lv_year.
*      IF lv_year >= 2019.
        ch_set = VALUE #( BASE ch_set ( year = lv_year ) ).
*      ENDIF.
      ADD 1 TO lv_year.
    ENDWHILE.

*      ENDIF.
*    ELSE.
*      ch_set = VALUE #( ( year = lv_curr_year ) ).
*    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->GOALSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_GOAL
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD goalset_get_entityset.
    DATA: lt_kpi     TYPE TABLE OF zhrt_ui_kpi,
          l_uname    TYPE uname,
          r_tab      TYPE tttext,
          l_textname TYPE thead-tdname.

    DATA(lt_keys) = io_tech_request_context->get_source_keys( ).
    TRY.
        DATA(l_reqid) = lt_keys[ name = 'REQID' ]-value.
        l_uname = lt_keys[ name = 'UNAME' ]-value.
      CATCH cx_root.

    ENDTRY.

    SELECT * FROM zhrt_ui_kpi INTO CORRESPONDING FIELDS OF TABLE @et_entityset
            WHERE reqid EQ @l_reqid.

    LOOP AT et_entityset ASSIGNING FIELD-SYMBOL(<ent>).
      l_textname = |{ <ent>-reqid }{ l_uname }{ <ent>-purposeid }|.
*      CLEAR r_tab[].
*      CALL FUNCTION 'READ_TEXT'
*        EXPORTING
*          id                      = 'ZKPI'
*          language                = sy-langu
*          name                    = l_textname
*          object                  = 'Z_FIO_KPI'
*        TABLES
*          lines                   = r_tab[]
*        EXCEPTIONS
*          id                      = 1
*          language                = 2
*          name                    = 3
*          not_found               = 4
*          object                  = 5
*          reference_check         = 6
*          wrong_access_to_archive = 7
*          OTHERS                  = 8.
*      LOOP AT r_tab ASSIGNING FIELD-SYMBOL(<str>).
*        IF sy-tabix EQ 1.
*          <ent>-text = <str>-tdline.
*        ELSE.
*          <ent>-text = |{ <ent>-text } { <str>-tdline }|.
*        ENDIF.
*      ENDLOOP.
      CALL METHOD read_text(
        IMPORTING
          text_id = l_textname
        CHANGING
          text    = <ent>-text ).
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->GROUPSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_GROUP
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD groupset_get_entityset.
    DATA: pernr    TYPE persno,
          lt_p0001 TYPE TABLE OF p0001,
          _objec   TYPE TABLE OF objec,
          _struc   TYPE TABLE OF struc,
          orgeh    TYPE orgeh,
          l_uname  TYPE uname.
*    DATA(lt_filter) = io_tech_request_context->get_filter( )->get_filter_select_options( ).
*    DATA(r_year) = VALUE #( lt_filter[ property = 'YEAR' ]-select_options OPTIONAL ).
*
*    DATA(r_us) = VALUE #( lt_filter[ property = 'UNAME' ]-select_options OPTIONAL ).
*    l_uname = VALUE #( r_us[ 1 ]-low OPTIONAL ).
    DATA(lt_keys) = io_tech_request_context->get_source_keys( ).

    l_uname = VALUE #( lt_keys[ name = 'UNAME' ]-value ).
*        l_year = lt_keys[ name = 'YEAR' ]-value.
    DATA(l_adm) = VALUE #( lt_keys[ name = 'ADMIN' ]-value ).

    IF l_uname IS INITIAL.
      l_uname = sy-uname.
    ENDIF.
    CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
      EXPORTING
        user_name               = l_uname
      IMPORTING
        employeenumber          = pernr
      EXCEPTIONS
        no_employeenumber_found = 1
        subtype_not_available   = 2
        OTHERS                  = 3.

    m_read_inf pernr '0001' lt_p0001 sy-datum.   " ins доработка даты NaumovSM
*    m_read_inf pernr '0001' lt_p0001.   " del доработка даты NaumovSM
    TRY.
*        orgeh = lt_p0001[ 1 ]-orgeh.
*        orgeh = VALUE #( lt_p0001[ 1 ]-orgeh OPTIONAL ).
        orgeh = get_oe( im_plans = lt_p0001[ 1 ]-plans )."lt_p0001[ 1 ]-orgeh.
      CATCH cx_root.
        EXIT.
    ENDTRY.

* Для администратора заменяем корневую ОЕ и ТН
    IF l_adm EQ abap_true.
*      get_oe_admin( EXPORTING
*                      im_orgeh = orgeh
*                    IMPORTING
*                      ex_pernr = DATA(l_pernr)
*                      ex_orgeh = DATA(l_orgeh)
*                    ).
*      pernr = l_pernr.
*      orgeh = l_orgeh.
*      pernr = '00003801'.
      orgeh = '01000001'.
    ENDIF.

    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype       = c_otype_o
        act_objid       = orgeh
        act_wegid       = c_wegid_o
        act_plvar       = c_plvar
        act_begda       = sy-datum
        act_endda       = sy-datum
        authority_check = ''
      TABLES
        result_struc    = _struc
        result_objec    = _objec
      EXCEPTIONS
        no_plvar_found  = 1
        no_entry_found  = 2
        OTHERS          = 3.
    LOOP AT _objec ASSIGNING FIELD-SYMBOL(<obj>).
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<entity>).
      <entity>-key = <obj>-objid.
      <entity>-ename = <obj>-stext.
      <entity>-collapsed = abap_false."abap_true.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->LINESET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_LINE
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD lineset_get_entityset.
    DATA: lt_entityset TYPE zcl_zhr_kpi_app_mpc=>tt_person.

    DEFINE m_get_objec.
      CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
        EXPORTING
          otype  = &1
          objid  = CONV objektid( &2 )
*          datum  = sy-datum "Del Naumov
          datum  = &4 "Ins Naumov
        CHANGING
          result = &3
        EXCEPTIONS
          OTHERS = 4.
    END-OF-DEFINITION.
    DEFINE m_read_inf.
      REFRESH: &3.
      CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          pernr           = CONV persno( &1 )
          infty           = &2
*          begda           = sy-datum   " del NaumovSM
*          endda           = sy-datum   " del NaumovSM
          begda           = &4   " ins NaumovSM
          endda           = &4   " ins NaumovSM
        TABLES
          infty_tab       = &3
        EXCEPTIONS
          infty_not_found = 1
          invalid_input   = 2
          OTHERS          = 3.
    END-OF-DEFINITION.

    get_person_set(
         EXPORTING io_tech_request_context = io_tech_request_context
          CHANGING ch_set = lt_entityset ).


    LOOP AT lt_entityset ASSIGNING FIELD-SYMBOL(<ent>).
      IF <ent>-parentid IS NOT INITIAL AND <ent>-id IS NOT INITIAL AND <ent>-parentid NE <ent>-id.
        APPEND VALUE #( from = <ent>-parentid to = <ent>-id ) TO et_entityset.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->MULTIPLEAPPROVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_REQID_STR                   TYPE        STRING
* | [--->] IM_UNAME                       TYPE        UNAME
* | [<---] EX_FLAG_OK                     TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD multipleapprove.
    DATA: lt_reqid            TYPE TABLE OF pru_char255,
          lr_reqid            TYPE RANGE OF zhrt_ui_kpi_gra-reqid,
          ls_new_head_request TYPE zhrt_ui_kpi_gra,
          l_textname          TYPE thead-tdname,
          l_text              TYPE string,
          lt_deleg            TYPE zhr_uname_t,
          l_start             TYPE uname,
          l_text_out          TYPE zhre_ui_comment_long,
          lt_route            TYPE zhr_ui_approver_t.
    SPLIT im_reqid_str AT ',' INTO TABLE lt_reqid.
    LOOP AT lt_reqid ASSIGNING FIELD-SYMBOL(<req>).
      CONDENSE <req> NO-GAPS.
      IF <req> IS NOT INITIAL.   " проверяем на наличие данных
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <req> ) TO lr_reqid.
      ENDIF.   " проверяем на наличие данных
    ENDLOOP.

    CHECK lr_reqid IS NOT INITIAL.   " проверяем на наличие данных

    SELECT *
      FROM zhrt_ui_kpi_gra AS gra
      INTO TABLE @DATA(lt_grade)
     WHERE reqid IN @lr_reqid.

    LOOP AT lt_grade ASSIGNING FIELD-SYMBOL(<grade>).
      CLEAR: ls_new_head_request.
      IF <grade>-declinecommentary IS INITIAL.
        l_textname = |{ <grade>-reqid }D{ <grade>-rdate }{ <grade>-rtime }|.
        CALL METHOD read_text(
          IMPORTING
            text_id = l_textname
          CHANGING
            text    = l_text ).
        <grade>-declinecommentary = l_text.
      ENDIF.

      IF <grade>-percentcommentary IS INITIAL.
        l_textname = |{ <grade>-reqid }P{ <grade>-rdate }{ <grade>-rtime }|.
        CALL METHOD read_text(
          IMPORTING
            text_id = l_textname
          CHANGING
            text    = l_text ).
        <grade>-percentcommentary = l_text.
      ENDIF.


      CLEAR: l_start.
      MOVE-CORRESPONDING <grade> TO ls_new_head_request.
      ls_new_head_request-aenam = sy-uname.
* { ins NaumovSM 25.03.2021
      IF im_uname <> sy-uname.
        ls_new_head_request-deleg = sy-uname.
        ls_new_head_request-aenam = im_uname.
      ENDIF.
* } ins NaumovSM 25.03.2021
      ls_new_head_request-rdate = sy-datum.
      ls_new_head_request-rtime = sy-timlo.
*      IF im_uname NE sy-uname.
*        get_deleg_tab( EXPORTING im_uname = sy-uname
*                        CHANGING ch_uname_tab = lt_deleg ).
*        CLEAR l_start.
*        READ TABLE lt_deleg WITH KEY table_line = l_user INTO l_start.
*        ls_new_head_request-approver = get_leader( im_uname = l_start ).
*      ELSE.
*        ls_new_head_request-approver = get_leader( im_uname = im_uname ).
*      ENDIF.


      ls_new_head_request-approver = get_leader( im_uname = im_uname ).
* Получаем полный маршрут согласования
      get_appr_route( EXPORTING
                        im_author = ls_new_head_request-uname
                        iv_year = <grade>-yeara
                      CHANGING
                        ch_tab = lt_route ).

* { ins NaumovSM  Проверка признака исключения топа ZHR_FIO_GRA_TOP
      IF lt_route IS NOT INITIAL.
        READ TABLE lt_route WITH KEY top = abap_true ASSIGNING FIELD-SYMBOL(<ls_del_top_route>).
        IF sy-subrc EQ 0.
          IF <ls_del_top_route>-orgeh IN zcl_tvarvc=>read( i_name = 'ZHR_FIO_GRA_TOP' i_type = 'S' ).
            DELETE lt_route INDEX ( lines( lt_route ) ).
          ENDIF.
        ENDIF.
      ENDIF.
* } ins NaumovSM

      READ TABLE lt_route WITH KEY uname = im_uname ASSIGNING FIELD-SYMBOL(<route>).
      IF sy-subrc EQ 0.
        DATA(l_idx) = sy-tabix + 1.
        TRY.
            READ TABLE lt_route INDEX l_idx ASSIGNING <route>.
            IF sy-subrc EQ 0.
              ls_new_head_request-approver = <route>-uname.
            ENDIF.
          CATCH cx_root.
        ENDTRY.
      ENDIF.

      IF check_top_man( im_uname = im_uname ) = abap_true.
        ls_new_head_request-status = c_st_03.
*        ls_new_head_request-approver = im_uname.
        CLEAR: ls_new_head_request-approver.
      ELSE.
        ls_new_head_request-status = c_st_02.
      ENDIF.
      CLEAR: ls_new_head_request-declinecommentary, ls_new_head_request-percentcommentary.
      IF sy-subrc EQ 0.
        MODIFY zhrt_ui_kpi_gra FROM ls_new_head_request.
      ENDIF.
      TRY.
          CALL FUNCTION 'ZHRT_UI_KPI_GRA_WRITE_DOCUMENT'
            EXPORTING
              objectid            = CONV cdobjectv( ls_new_head_request-reqid )
              tcode               = sy-tcode
              utime               = sy-uzeit
              udate               = sy-datum
              username            = sy-uname
*             PLANNED_CHANGE_NUMBER            = ' '
*             OBJECT_CHANGE_INDICATOR          = 'U'
*             PLANNED_OR_REAL_CHANGES          = ' '
*             NO_CHANGE_POINTERS  = ' '
*             UPD_ICDTXT_ZHRT_UI_KPI_GRA       = 'I'
              n_zhrt_ui_kpi_gra   = ls_new_head_request
              o_zhrt_ui_kpi_gra   = <grade>
              upd_zhrt_ui_kpi_gra = 'U'.
          CALL FUNCTION 'ZHR_RN_REQUEST_SEND_MAIL'
            IN BACKGROUND TASK
            EXPORTING
              numb     = CONV zhre_ui5_req_numb( ls_new_head_request-reqid )
              flag_kpi = abap_true.
        CATCH cx_root INTO DATA(lo_error).
          DATA(lv_dummy) = lo_error->get_text( ).
      ENDTRY.

      l_textname = |{ ls_new_head_request-reqid }D{ ls_new_head_request-rdate }{ ls_new_head_request-rtime }|.
      l_text_out = <grade>-declinecommentary.
      CALL METHOD write_text(
        IMPORTING
          text_id = l_textname
          text    = l_text_out ).
      CLEAR l_text.
      l_textname = |{ ls_new_head_request-reqid }P{ ls_new_head_request-rdate }{ ls_new_head_request-rtime }|.
      l_text_out = <grade>-percentcommentary.
      CALL METHOD write_text(
        IMPORTING
          text_id = l_textname
          text    = l_text_out ).

    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->MULTIPLESUBMIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IM_REQID_STR                   TYPE        STRING
* | [--->] IM_UNAME                       TYPE        UNAME
* | [<---] EX_FLAG_OK                     TYPE        FLAG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD multiplesubmit.

    DATA: lt_reqid            TYPE TABLE OF pru_char255,
          lr_reqid            TYPE RANGE OF zhrt_ui_kpi_gra-reqid,
          ls_new_head_request TYPE zhrt_ui_kpi_gra,
          l_textname          TYPE thead-tdname,
          l_text              TYPE string,
          lt_deleg            TYPE zhr_uname_t,
          l_start             TYPE uname,
          l_text_out          TYPE zhre_ui_comment_long.
*          lt_route            TYPE zhr_ui_approver_t.

    CHECK check_admin( im_uname = im_uname ) = abap_true.

    SPLIT im_reqid_str AT ',' INTO TABLE lt_reqid.

    LOOP AT lt_reqid ASSIGNING FIELD-SYMBOL(<req>).
      CONDENSE <req> NO-GAPS.
      IF <req> IS NOT INITIAL.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <req> ) TO lr_reqid.
      ENDIF.
    ENDLOOP.

    CHECK lr_reqid IS NOT INITIAL.

    SELECT *
      FROM zhrt_ui_kpi_gra AS gra
      INTO TABLE @DATA(lt_grade)
     WHERE reqid IN @lr_reqid.

    LOOP AT lt_grade ASSIGNING FIELD-SYMBOL(<grade>).

      CHECK <grade>-status = c_st_03.

      IF <grade>-declinecommentary IS INITIAL.
        l_textname = |{ <grade>-reqid }D{ <grade>-rdate }{ <grade>-rtime }|.
        CALL METHOD read_text(
          IMPORTING
            text_id = l_textname
          CHANGING
            text    = l_text ).
        <grade>-declinecommentary = l_text.
      ENDIF.

      IF <grade>-percentcommentary IS INITIAL.
        l_textname = |{ <grade>-reqid }P{ <grade>-rdate }{ <grade>-rtime }|.
        CALL METHOD read_text(
          IMPORTING
            text_id = l_textname
          CHANGING
            text    = l_text ).
        <grade>-percentcommentary = l_text.
      ENDIF.

      CLEAR: l_start.
      MOVE-CORRESPONDING <grade> TO ls_new_head_request.
      ls_new_head_request-aenam = sy-uname.
* { ins NaumovSM 25.03.2021
      IF im_uname <> sy-uname.
        ls_new_head_request-deleg = sy-uname.
        ls_new_head_request-aenam = im_uname.
      ENDIF.
* } ins NaumovSM 25.03.2021
      ls_new_head_request-rdate = sy-datum.
      ls_new_head_request-rtime = sy-timlo.

*      ls_new_head_request-approver = get_leader( im_uname = im_uname ).
*
* Получаем полный маршрут согласования
*      get_appr_route( EXPORTING
*                        im_author = ls_new_head_request-uname
*                      CHANGING
*                        ch_tab = lt_route ).
*
*      READ TABLE lt_route WITH KEY uname = im_uname ASSIGNING FIELD-SYMBOL(<route>).
*      IF sy-subrc EQ 0.
*        DATA(l_idx) = sy-tabix + 1.
*        TRY.
*            READ TABLE lt_route INDEX l_idx ASSIGNING <route>.
*            IF sy-subrc EQ 0.
*              ls_new_head_request-approver = <route>-uname.
*            ENDIF.
*          CATCH cx_root.
*        ENDTRY.
*      ENDIF.

*     CLEAR ls_new_head_request-approver.
*    ENDIF.

      ls_new_head_request-status = c_st_06.

      CLEAR: ls_new_head_request-declinecommentary, ls_new_head_request-percentcommentary.
      IF sy-subrc EQ 0.
        MODIFY zhrt_ui_kpi_gra FROM ls_new_head_request.
      ENDIF.

      TRY.
          CALL FUNCTION 'ZHRT_UI_KPI_GRA_WRITE_DOCUMENT'
            EXPORTING
              objectid            = CONV cdobjectv( ls_new_head_request-reqid )
              tcode               = sy-tcode
              utime               = sy-uzeit
              udate               = sy-datum
              username            = sy-uname
              n_zhrt_ui_kpi_gra   = ls_new_head_request
              o_zhrt_ui_kpi_gra   = <grade>
              upd_zhrt_ui_kpi_gra = 'U'.

          CALL FUNCTION 'ZHR_RN_REQUEST_SEND_MAIL'
            IN BACKGROUND TASK
            EXPORTING
              numb     = CONV zhre_ui5_req_numb( ls_new_head_request-reqid )
              flag_kpi = abap_true.

        CATCH cx_root INTO DATA(lo_error).

          DATA(lv_dummy) = lo_error->get_text( ).

      ENDTRY.

      l_textname = |{ ls_new_head_request-reqid }D{ ls_new_head_request-rdate }{ ls_new_head_request-rtime }|.
      l_text_out = <grade>-declinecommentary.
      CALL METHOD write_text(
        IMPORTING
          text_id = l_textname
          text    = l_text_out ).
      CLEAR l_text.
      l_textname = |{ ls_new_head_request-reqid }P{ ls_new_head_request-rdate }{ ls_new_head_request-rtime }|.
      l_text_out = <grade>-percentcommentary.
      CALL METHOD write_text(
        IMPORTING
          text_id = l_textname
          text    = l_text_out ).

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->PERSONSET_GET_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IO_REQUEST_OBJECT              TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [<---] ER_ENTITY                      TYPE        ZCL_ZHR_KPI_APP_MPC=>TS_PERSON
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD personset_get_entity.
    DATA: lt_kpi      TYPE TABLE OF zhrt_ui_kpi,
          l_uname     TYPE uname,
          lt_p0105    TYPE TABLE OF p0105,
          _bipernr    TYPE REF TO zhr_objbif_pernr,
          _biplans    TYPE REF TO zhr_objbif_plans,
          _biorgeh    TYPE REF TO zhr_objbif_orgeh,
          l_pernr     TYPE persno,
          l_year      TYPE gjahr,
          l_date      TYPE dats,
          lt_deleg    TYPE zhr_uname_t,
          l_user      TYPE uname,
          l_textname  TYPE thead-tdname,
          l_collapsed TYPE zhre_ui_collapsed_text,
          l_text      TYPE string.
* { ins NaumovSM проверка на уволенного
    DATA: lt_hire_fire_pernr TYPE TABLE OF phifi
        , lt_history         TYPE TABLE OF cdred
        .
* } ins NaumovSM проверка на уволенного
    DATA: lo_message_container TYPE REF TO /iwbep/if_message_container.
* Макрос для считывания инфотипов
    DEFINE m_read_inf.
      REFRESH: &3.
      CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          pernr           = CONV persno( &1 )
          infty           = &2
          begda           = &4   " ins NaumovSM
          endda           = &4   " ins NaumovSM
*          begda           = sy-datum   " del NaumovSM
*          endda           = sy-datum   " del NaumovSM
        TABLES
          infty_tab       = &3
        EXCEPTIONS
          infty_not_found = 1
          invalid_input   = 2
          OTHERS          = 3.
    END-OF-DEFINITION.

    DATA l TYPE string.
    DATA(lt_keys) = io_tech_request_context->get_keys( ).
    DATA(lo_request) = me->mr_request_details.
    DATA(lt_url_param) = lo_request->t_uri_query_parameter.
    READ TABLE lt_url_param ASSIGNING FIELD-SYMBOL(<param>) WITH KEY name = 'Delegate'.
    IF sy-subrc EQ 0.
      l_user = <param>-value.
    ELSE.
      l_user = sy-uname.
    ENDIF.
    READ TABLE lt_url_param ASSIGNING <param> WITH KEY name = 'Collapsed'.
    IF sy-subrc EQ 0.
      l_collapsed = <param>-value.
    ENDIF.
    TRY.
        l_uname = lt_keys[ name = 'UNAME' ]-value.
        l_year = lt_keys[ name = 'YEAR' ]-value.
        l_date = |{ l_year }1231|.   " если сотрудник не уволен, иначе переопределяем дату ниже
      CATCH cx_root.

    ENDTRY.

    get_deleg_tab( EXPORTING im_uname = sy-uname
                    CHANGING ch_uname_tab = lt_deleg ).
    IF l_user NE sy-uname.
      READ TABLE lt_deleg WITH KEY table_line = l_user TRANSPORTING NO FIELDS.
      CHECK sy-subrc EQ 0.
    ENDIF.
    CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
      EXPORTING
        user_name               = l_uname
      IMPORTING
        employeenumber          = l_pernr
      EXCEPTIONS
        no_employeenumber_found = 1
        subtype_not_available   = 2
        OTHERS                  = 3.
    er_entity-uname = l_uname.
    er_entity-id = l_pernr.
    CLEAR: _bipernr, _biplans, _biorgeh.
* { ins NaumovSM Добавляем анализ на уволенность сотрудника (для просмотра в будущем)
    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HRF_HIRE_FIRE'
      EXPORTING
        pernr         = l_pernr
*       BEGDA         = '18000101'
*       ENDDA         = '99991231'
*       LANGUAGE      =
*       PERSON        =
      TABLES
        ms_data       = lt_hire_fire_pernr
      EXCEPTIONS
        reject_pernr  = 1
        display_error = 2
        OTHERS        = 3.
    IF sy-subrc = 0.
      SORT lt_hire_fire_pernr DESCENDING BY endda.
      LOOP AT lt_hire_fire_pernr ASSIGNING FIELD-SYMBOL(<ls_hire_fire>)
                                     WHERE stat2 = '3'.
        l_date = <ls_hire_fire>-endda.
        EXIT.
      ENDLOOP.
* Implement suitable error handling here
    ENDIF.
* } ins NaumovSM

    CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
      EXPORTING
        otype  = c_otype_p
        objid  = l_pernr
*        datum  = sy-datum   " del NaumovSM
        datum  = l_date   " ins NaumovSM
      CHANGING
        result = _bipernr
      EXCEPTIONS
        OTHERS = 1.
    IF _bipernr IS NOT INITIAL.
*      er_entity-ename = _bipernr->get_name( adatum = sy-datum anoauth = abap_true ).   " del NaumovSM
      er_entity-ename = _bipernr->get_name( adatum = l_date anoauth = abap_true ).   " ins NaumovSM
      TRY.
          _bipernr->get_orgass(
            EXPORTING
              adatum = l_date
            IMPORTING
              resplans = _biplans
              resorgeh = _biorgeh
        ).
*         Наименование должности
          IF _biplans IS NOT INITIAL.
*                  er_entity-stell = _biplans->objec-objid.
            er_entity-plnam = _biplans->get_name( adatum = l_date anoauth = abap_true ).
            TRANSLATE er_entity-plnam+0(1) TO UPPER CASE.
          ENDIF.
          IF _biorgeh IS NOT INITIAL.
            er_entity-orgna = _biorgeh->get_name( adatum = l_date anoauth = abap_true ).
            DATA: _orgpath  TYPE zhr_objbif_objec_orgpath_t,
                  it_attrib TYPE TABLE OF pt1222.
            _biorgeh->get_orgpath(
                       EXPORTING
                         aworigin = 'X'
*                             aminlevel = 3
                         adatum = l_date
                       IMPORTING
                         result = _orgpath[]
                       ).
            SORT _orgpath BY pobid DESCENDING.
            LOOP AT _orgpath ASSIGNING FIELD-SYMBOL(<path>).
              CLEAR _biorgeh.
              CALL FUNCTION 'ZHR_EBDS_OBJBIF_GET'
                EXPORTING
                  otype  = c_otype_o
                  objid  = <path>-pobid
                  datum  = l_date
                CHANGING
                  result = _biorgeh
                EXCEPTIONS
                  OTHERS = 1.
              CLEAR: it_attrib[].
              CALL FUNCTION 'RH_OM_ATTRIBUTES_READ'
                EXPORTING
                  plvar            = c_plvar
                  otype            = c_otype_o
                  objid            = <path>-pobid
                  seldate          = l_date
                TABLES
                  attrib           = it_attrib
                EXCEPTIONS
                  no_active_plvar  = 1
                  no_attributes    = 2
                  no_values        = 3
                  object_not_found = 4
                  OTHERS           = 5.
              IF er_entity-dirname IS INITIAL.
                READ TABLE it_attrib ASSIGNING FIELD-SYMBOL(<attr>) WITH KEY attrib = 'ZLEVEL' low = 'УП'.
                IF sy-subrc EQ 0.
                  er_entity-dirname = _biorgeh->get_name( adatum = l_date anoauth = abap_true ).
                ENDIF.
              ENDIF.
              IF er_entity-podrname IS INITIAL.
                READ TABLE it_attrib ASSIGNING <attr> WITH KEY attrib = 'ZLEVEL' low = 'ДП'.
                IF sy-subrc EQ 0.
                  er_entity-podrname = _biorgeh->get_name( adatum = l_date anoauth = abap_true ).
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        CATCH cx_root.
      ENDTRY.

    ENDIF.

    SELECT SINGLE grade1, grade2, procent, status, yeara, reqid, st~stnam, declinecommentary, percentcommentary, aenam, approver, uname, rdate, rtime
      FROM zhrt_ui_kpi_gra AS gra
 LEFT JOIN zhrt_ui_leave_st AS st ON gra~status = st~ident
      INTO @DATA(ls_gr)
     WHERE uname = @l_uname
       AND yeara EQ @l_year.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_gr TO er_entity.
*      er_entity-grade1 = er_entity-grade1.
*      er_entity-grade2 = er_entity-grade2.
      er_entity-year = ls_gr-yeara.
      IF ls_gr-status NE '04'.
*      READ TABLE lt_deleg WITH KEY table_line = ls_gr-aenam TRANSPORTING NO FIELDS.
*      IF sy-subrc EQ 0.
*        DATA(lf_ae) = abap_true.
*      ENDIF.
*      READ TABLE lt_deleg WITH KEY table_line = ls_gr-approver TRANSPORTING NO FIELDS.
*      IF sy-subrc EQ 0.
*        DATA(lf_ap) = abap_true.
*      ENDIF.
*      READ TABLE lt_deleg WITH KEY table_line = ls_gr-uname TRANSPORTING NO FIELDS.
*      IF sy-subrc EQ 0.
*        DATA(lf_un) = abap_true.
*      ENDIF.
        IF ls_gr-aenam EQ l_user AND ls_gr-approver NE l_user.
          er_entity-crossflag = abap_true.
          er_entity-accessback = abap_true.
        ENDIF.
        IF ls_gr-approver EQ l_user.
          er_entity-accessappr = abap_true.
          er_entity-crossflag = abap_true.
*        er_entity-accessback = abap_true.
        ENDIF.
        IF ls_gr-uname EQ l_user.
          CLEAR er_entity-accessback.
        ENDIF.
      ENDIF.
      IF ls_gr-status EQ '03'.
        IF ls_gr-aenam EQ l_user OR ls_gr-approver EQ l_user OR ls_gr-aenam EQ sy-uname OR ls_gr-approver EQ sy-uname.
          er_entity-crossflag = abap_true.
          er_entity-accessback = abap_true.
          er_entity-accessappr = abap_false.
        ENDIF.
      ENDIF.
    ELSE.
      er_entity-grade1 = er_entity-grade2 = 2.
      er_entity-procent = 100.
      CONDENSE er_entity-procent NO-GAPS.
      er_entity-status = '01'.
      er_entity-stnam = 'В работе'.
      er_entity-year = l_year.
      ls_gr-uname = l_uname.
    ENDIF.
    IF er_entity-status EQ c_st_06.
      er_entity-crossflag = abap_true.
    ENDIF.
*{ 06.01.2020 Захаренко А.С.
*    IF l_year NE sy-datum+0(4).
*      CLEAR er_entity-status.
*      er_entity-stnam = 'Закрыто'.
*    ENDIF.
*{ 06.01.2020 Захаренко А.С.
    er_entity-collapsed = l_collapsed.

    IF er_entity-declinecommentary IS INITIAL.
      CLEAR l_text.
      l_textname = |{ ls_gr-reqid }D{ ls_gr-rdate }{ ls_gr-rtime }|.
      CALL METHOD read_text(
        IMPORTING
          text_id = l_textname
        CHANGING
          text    = l_text ).
      er_entity-declinecommentary = l_text.
    ENDIF.

    IF er_entity-percentcommentary IS INITIAL.
      CLEAR l_text.
      l_textname = |{ ls_gr-reqid }P{ ls_gr-rdate }{ ls_gr-rtime }|.
      CALL METHOD read_text(
        IMPORTING
          text_id = l_textname
        CHANGING
          text    = l_text ).
      er_entity-percentcommentary = l_text.
    ENDIF.

*    DATA(lo_request) = me->mr_request_details.
*    DATA(lt_url_param) = lo_request->t_uri_query_parameter.
    READ TABLE lt_url_param ASSIGNING <param> WITH KEY name = 'admin'.
    IF sy-subrc EQ 0.
      DATA(l_adm) = <param>-value.
    ENDIF.

    IF l_adm EQ 'true'.
      IF check_admin( im_uname = sy-uname ) EQ abap_false.

        CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
          RECEIVING
            ro_message_container = lo_message_container.
        CALL METHOD lo_message_container->add_message
          EXPORTING
            iv_msg_type               = /iwbep/cl_cos_logger=>warning
            iv_msg_id                 = 'ZHR_UI5'
*           iv_msg_number             = '002'   " <-- del NaumovSM 14082
            iv_msg_number             = '003'   " <-- ins NaumovSM 14082
            iv_add_to_response_header = abap_true.
        CLEAR er_entity.
      ELSE.
        CLEAR: er_entity-accessappr, er_entity-accessback.
        er_entity-crossflag = abap_true.
        IF er_entity-status EQ c_st_06.
          er_entity-accs_back = abap_true.
        ELSEIF er_entity-status EQ c_st_03.
          er_entity-accs_accept = abap_true.
        ENDIF.
      ENDIF.
    ELSE.
      IF ls_gr-uname NE l_user
        AND ls_gr-aenam NE l_user
        AND ls_gr-approver NE l_user
        AND ls_gr-uname NE sy-uname
        AND ls_gr-aenam NE sy-uname
        AND ls_gr-approver NE sy-uname.
        CHECK er_entity-status NE c_st_06.
* <-- del NaumovSM 14.12.2020
**        DATA: lo_message_container TYPE REF TO /iwbep/if_message_container.
*        CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
*          RECEIVING
*            ro_message_container = lo_message_container.
*        CALL METHOD lo_message_container->add_message
*          EXPORTING
*            iv_msg_type               = /iwbep/cl_cos_logger=>warning
*            iv_msg_id                 = 'ZHR_UI5'
**           iv_msg_number             = '002'   " <-- del NaumovSM 14082
*            iv_msg_number             = '003'   " <-- ins NaumovSM 14082
*            iv_add_to_response_header = abap_true.
*        CLEAR er_entity.
* --> del NaumovSM 14.12.2020
        er_entity-accessappr = abap_false.   " <-- ins NaumovSM 14.12.2020
      ENDIF.
    ENDIF.
    IF l_year < '2019'.
      CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
        RECEIVING
          ro_message_container = lo_message_container.
      CALL METHOD lo_message_container->add_message
        EXPORTING
          iv_msg_type               = /iwbep/cl_cos_logger=>warning
          iv_msg_id                 = 'ZHR_UI5'
*         iv_msg_number             = '002'   " <-- del NaumovSM 14082
          iv_msg_number             = '003'   " <-- ins NaumovSM 14082
          iv_add_to_response_header = abap_true.
      CLEAR er_entity.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->PERSONSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_PERSON
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD personset_get_entityset.
    get_person_set(
     EXPORTING io_tech_request_context = io_tech_request_context
      CHANGING ch_set = et_entityset ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_ZHR_KPI_APP_DPC_EXT=>READ_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [<---] TEXT_ID                        TYPE        THEAD-TDNAME
* | [<-->] TEXT                           TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD read_text.
    DATA: r_tab      TYPE tttext.
    CLEAR r_tab[].
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ZKPI'
        language                = sy-langu
        name                    = text_id
        object                  = 'Z_FIO_KPI'
      TABLES
        lines                   = r_tab[]
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    LOOP AT r_tab ASSIGNING FIELD-SYMBOL(<str>).
      IF sy-tabix EQ 1.
        text = <str>-tdline.
      ELSE.
        text = |{ text } { <str>-tdline }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Private Method ZCL_ZHR_KPI_APP_DPC_EXT=>SEND_NOSES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IMPER                          TYPE        PERSNO
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD send_noses.
    DATA: l_subj       TYPE string,
          lt_text      TYPE soli_tab,
          lt_mail_user TYPE TABLE OF ad_smtpadr,
          lt_p0105     TYPE TABLE OF p0105,
          _xmlstr      TYPE string,
          l_url        TYPE ehfnd_dms_url,
          l_sendler    TYPE adr6-smtp_addr.
    DATA: BEGIN OF ls_url,
            url TYPE ehfnd_dms_url,
          END OF ls_url.
    IF sy-sysid NE 'HR2'.
      EXIT.
    ENDIF.
*    ls_url-url = 'http://saphr6.msk.rn.ru:8123/sap/bc/ui5_ui5/ui2/ushell/shells/abap/FioriLaunchpad.html?sap-client=250&sap-language=RU#ZHR_TRANSACTION-diplay'.
    ls_url-url = gen_url( ).
    ls_url-url = |{ ls_url-url }#ZHR_TRANSACTION-display|.
    TRY.
        CALL TRANSFORMATION zhr_ui_approver_noses
                     SOURCE root = ls_url
                     RESULT XML  _xmlstr.
      CATCH cx_root.
        EXIT.
    ENDTRY.
* Убираю самый первый тег и разбиваю в таблицу
    _xmlstr = _xmlstr+40.
    CALL FUNCTION 'ZHR_SLPIT_STRING'
      EXPORTING
        string        = _xmlstr
        length        = 250
        use_separator = '#'
      TABLES
        split         = lt_text.

* Запрос на отправку
    DATA(lo_send_request) = cl_bcs=>create_persistent( ).
* Создание письма
    lo_send_request->set_document(
                      cl_document_bcs=>create_document(
                        i_type    = 'HTM'
                        i_subject = ''
                        i_text    = lt_text
                      )
                    ).
* Отправитель
    l_sendler = TEXT-009.
    lo_send_request->set_sender( i_sender = cl_cam_address_bcs=>create_internet_address( i_address_string = l_sendler ) ).
* Задаем тему письма
    lo_send_request->set_message_subject( l_subj ).

    CLEAR lt_p0105[].
    CALL FUNCTION 'HR_READ_INFOTYPE_AUTHC_DISABLE'.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = imper
        infty           = '0105'
        begda           = sy-datum
        endda           = sy-datum
      TABLES
        infty_tab       = lt_p0105
      EXCEPTIONS
        infty_not_found = 1
        invalid_input   = 2
        OTHERS          = 3.
    READ TABLE lt_p0105 WITH KEY subty = c_mail ASSIGNING FIELD-SYMBOL(<p0105>).
    IF sy-subrc EQ 0.
      SPLIT <p0105>-usrid_long AT ';' INTO TABLE lt_mail_user.
    ENDIF.
    CHECK lt_mail_user[] IS NOT INITIAL.
*    lo_send_request->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address( i_address_string = 'TikhonovGI@sibintek.ru' ) ).
* Добавляем получателей
    LOOP AT lt_mail_user ASSIGNING FIELD-SYMBOL(<mail>).
      lo_send_request->add_recipient( i_recipient = cl_cam_address_bcs=>create_internet_address( i_address_string = <mail> ) ).
    ENDLOOP.
* Отправка
    lo_send_request->send( ).
    COMMIT WORK AND WAIT.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->SHORGEHSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_SHORGEH
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD shorgehset_get_entityset.
    DATA: _struc       TYPE TABLE OF struc,
          lt_p0001     TYPE TABLE OF p0001,
          pernr        TYPE persno,
          orgeh        TYPE orgeh,
          l_uname      TYPE uname,
          l_delegate   TYPE uname,
          _biorgeh     TYPE REF TO zhr_objbif_orgeh,
          l_cond_orgna TYPE string,
          l_queue      TYPE zhre_ui_queue,
          l_year       TYPE gjahr,
          l_orgeh      TYPE string.
*    IF me->mr_request_details->technical_request-navigation_path[ 1 ]-source_entity_type EQ 'Approver'.
*      CHECK sy-subrc EQ 0.
*    ENDIF.

    DATA(lt_keys) = io_tech_request_context->get_source_keys( ).
    l_delegate = VALUE #( lt_keys[ name = 'DELEGATE' ]-value OPTIONAL ).
    IF l_delegate IS INITIAL.
      l_delegate = sy-uname.
    ENDIF.
    l_uname = VALUE #( lt_keys[ name = 'UNAME' ]-value OPTIONAL ).
    IF l_uname IS INITIAL.
      l_uname = sy-uname.
    ENDIF.
    CHECK l_uname IS NOT INITIAL.
    l_queue = VALUE #( lt_keys[ name = 'QUEUE' ]-value OPTIONAL ).
    l_year = VALUE #( lt_keys[ name = 'YEAR' ]-value OPTIONAL ).
    DATA lt_appr TYPE TABLE OF zhrt_ui_approver.
    SELECT * FROM zhrt_ui_approver INTO TABLE lt_appr WHERE approver = l_delegate AND year_appr = l_year .

    DATA(lt_filter) = io_tech_request_context->get_filter( )->get_filter_select_options( ).
    DATA(r_orgna) = VALUE #( lt_filter[ property = 'ORGNA' ]-select_options OPTIONAL ).
    DATA(r_orgeh) = VALUE #( lt_filter[ property = 'ORGEH' ]-select_options OPTIONAL ).
    LOOP AT r_orgeh ASSIGNING FIELD-SYMBOL(<org>).
      <org>-option = 'EQ'.
    ENDLOOP.

    IF me->mr_request_details->technical_request-navigation_path[ 1 ]-source_entity_type EQ 'User'.
      CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
        EXPORTING
          user_name               = l_uname
        IMPORTING
          employeenumber          = pernr
        EXCEPTIONS
          no_employeenumber_found = 1
          subtype_not_available   = 2
          OTHERS                  = 3.

*      m_read_inf pernr '0001' lt_p0001.
      m_read_inf pernr '0001' lt_p0001 sy-datum.
      TRY.
*          orgeh = lt_p0001[ 1 ]-orgeh.
        orgeh = get_oe( im_plans = lt_p0001[ 1 ]-plans ).
        CATCH cx_root.
          EXIT.
      ENDTRY.

      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype       = c_otype_o
          act_objid       = orgeh
          act_wegid       = 'O-O_DOWN'
          act_tdepth      = '2'
          act_plvar       = c_plvar
          act_begda       = sy-datum
          act_endda       = sy-datum
          authority_check = ''
        TABLES
          result_struc    = _struc
        EXCEPTIONS
          no_plvar_found  = 1
          no_entry_found  = 2
          OTHERS          = 3.
    ELSEIF me->mr_request_details->technical_request-navigation_path[ 1 ]-source_entity_type EQ 'Approver'..
      LOOP AT lt_appr ASSIGNING FIELD-SYMBOL(<appr>) WHERE approver = l_delegate AND add_approver = l_uname AND queue = l_queue.
        CHECK <appr>-orgeh IS NOT INITIAL.
        APPEND INITIAL LINE TO _struc ASSIGNING FIELD-SYMBOL(<struc>).
        <struc>-objid = <appr>-orgeh.
      ENDLOOP.
    ENDIF.

    LOOP AT _struc ASSIGNING <struc>.
      IF r_orgeh[] IS NOT INITIAL.
        CHECK NOT <struc>-objid IN r_orgeh.
      ENDIF.
      CLEAR _biorgeh.
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<entity>).
*      m_get_objec c_otype_o <struc>-objid _biorgeh.   " del NaumovSM
      m_get_objec c_otype_o <struc>-objid _biorgeh sy-datum.   " ins NaumovSM
      <entity>-orgeh = <struc>-objid.
      CHECK _biorgeh IS NOT INITIAL.
      <entity>-orgna = _biorgeh->get_name( adatum = sy-datum anoauth = abap_true ).
      IF NOT <entity>-orgna IN r_orgna.
        DELETE TABLE et_entityset FROM <entity>.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->SHPERSSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_SHPERS
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD shpersset_get_entityset.
    DATA: l_werks      TYPE persa,
          lt_p0001     TYPE TABLE OF p0001,
          _bipernr     TYPE REF TO zhr_objbif_pernr,
          _biplans     TYPE REF TO zhr_objbif_plans,
          pernr        TYPE persno,
          l_cond_nachn TYPE string,
          l_cond_vorna TYPE string,
          l_cond_midnm TYPE string,
          lt_p0105     TYPE TABLE OF p0105,
          l_delegate   TYPE uname.

    DATA(lt_keys) = io_tech_request_context->get_source_keys( ).
    l_delegate = VALUE #( lt_keys[ name = 'UNAME' ]-value OPTIONAL ).
    IF l_delegate IS INITIAL.
      l_delegate = sy-uname.
    ENDIF.

    CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
      EXPORTING
        user_name               = sy-uname
      IMPORTING
        employeenumber          = pernr
      EXCEPTIONS
        no_employeenumber_found = 1
        subtype_not_available   = 2
        OTHERS                  = 3.
    DATA(lt_filter) = io_tech_request_context->get_filter( )->get_filter_select_options( ).
    DATA(r_nachn) = VALUE #( lt_filter[ property = 'NACHN' ]-select_options OPTIONAL ).
    DATA(r_vorna) = VALUE #( lt_filter[ property = 'VORNA' ]-select_options OPTIONAL ).
    DATA(r_midnm) = VALUE #( lt_filter[ property = 'MIDNM' ]-select_options OPTIONAL ).

*    REPLACE ALL OCCURRENCES OF '*' IN l_cond_nachn WITH '%'.
*    REPLACE ALL OCCURRENCES OF '*' IN l_cond_vorna WITH '%'.
*    REPLACE ALL OCCURRENCES OF '*' IN l_cond_midnm WITH '%'.
*    m_read_inf pernr '0001' lt_p0001.
*    TRY.
*        l_werks = lt_p0001[ 1 ]-werks.
*      CATCH cx_root.
*        EXIT.
*    ENDTRY.
    l_werks = '1000'.

* Используем SELECT для более быстрой выборки
    TYPES: BEGIN OF t_set,
             pernr TYPE persno,
             usrid TYPE uname,
             nachn TYPE pad_nachn,
             vorna TYPE pad_vorna,
             midnm TYPE pad_midnm,
             begda TYPE begda,
             endda TYPE endda,
             subty TYPE subty,
           END OF t_set.
    DATA lt_set TYPE TABLE OF t_set.
*    SELECT pa0001~pernr
*           pa0105~usrid
*           pa0002~nachn
*           pa0002~vorna
*           pa0002~midnm
*      FROM pa0001
*      LEFT JOIN pa0105 ON pa0105~pernr = pa0001~pernr
*     INNER JOIN pa0002 ON pa0002~pernr = pa0001~pernr
*      INTO TABLE lt_set
*     WHERE pa0001~begda <= sy-datum
*       AND pa0001~endda >= sy-datum
*       AND pa0001~werks = l_werks
*       AND pa0105~begda <= sy-datum
*       AND pa0105~endda >= sy-datum
*       AND pa0105~subty = c_subty_0105
*       AND pa0002~begda <= sy-datum
*       AND pa0002~endda >= sy-datum.
    SELECT pa0001~pernr
           pa0105~usrid
           pa0002~nachn
           pa0002~vorna
           pa0002~midnm
           pa0105~begda
           pa0105~endda
           pa0105~subty
      FROM pa0001
      LEFT JOIN pa0105 ON pa0105~pernr = pa0001~pernr
     INNER JOIN pa0002 ON pa0002~pernr = pa0001~pernr
      INTO TABLE lt_set
     WHERE pa0001~begda <= sy-datum
       AND pa0001~endda >= sy-datum
       AND pa0001~werks = l_werks
*             AND pa0105~begda <= sy-datum
*             AND pa0105~endda >= sy-datum
*             AND pa0105~subty = c_subty_0105
       AND pa0002~begda <= sy-datum
       AND pa0002~endda >= sy-datum.
    DELETE lt_set WHERE NOT ( begda <= sy-datum AND endda >= sy-datum ) OR subty NE c_subty_0105.

    SORT lt_set BY nachn ASCENDING vorna ASCENDING midnm ASCENDING.
* Фильтр по ФИО
    LOOP AT lt_set ASSIGNING FIELD-SYMBOL(<line>).
      IF NOT <line>-nachn IN r_nachn OR NOT <line>-vorna IN r_vorna OR NOT <line>-midnm IN r_midnm.
        DELETE lt_set WHERE pernr = <line>-pernr.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_set ASSIGNING <line>.
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<entity>).
      MOVE-CORRESPONDING <line> TO <entity>.
      <entity>-uname = <line>-usrid.
      CLEAR: _bipernr, _biplans.
*      m_get_objec c_otype_p <line>-pernr _bipernr.   " del NaumovSM
      m_get_objec c_otype_p <line>-pernr _bipernr sy-datum.   " ins NaumovSM
      TRY.
          _bipernr->get_orgass(
                EXPORTING
                  adatum = sy-datum
                IMPORTING
                  resplans = _biplans
            ).
          IF _biplans IS NOT INITIAL.
            <entity>-plnam = _biplans->get_name( adatum = sy-datum anoauth = abap_true ).
          ENDIF.
        CATCH cx_root.
      ENDTRY.
      <entity>-no_select = abap_true.
*      m_read_inf <line>-pernr '0105' lt_p0105.
      m_read_inf <line>-pernr '0105' lt_p0105 sy-datum.
      READ TABLE lt_p0105 WITH KEY subty = c_9009 zzflag = abap_true TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        CLEAR <entity>-no_select.
      ENDIF.
    ENDLOOP.

    SORT et_entityset BY nachn ASCENDING vorna ASCENDING midnm ASCENDING.
    DELETE et_entityset WHERE uname = l_delegate.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->SHQUEUESET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_SHQUEUE
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD shqueueset_get_entityset.
    DATA(lt_filter) = io_tech_request_context->get_filter( )->get_filter_select_options( ).
    DATA(r_queue) = VALUE #( lt_filter[ property = 'QUEUE' ]-select_options OPTIONAL ).
    IF 1 IN r_queue.
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<entity>).
      <entity>-queue = '1'.
      <entity>-queue_text = 'Передо мной'.
    ENDIF.
    IF 2 IN r_queue.
      APPEND INITIAL LINE TO et_entityset ASSIGNING <entity>.
      <entity>-queue = '2'.
      <entity>-queue_text = 'После меня'.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->SHYEARSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_SHYEAR
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD shyearset_get_entityset.
    DATA: l_year TYPE jahr.
    CONSTANTS c_year TYPE jahr VALUE '2019'.
    l_year = sy-datum+0(4).
    DO 10 TIMES.
      APPEND INITIAL LINE TO et_entityset ASSIGNING FIELD-SYMBOL(<line>).
      <line>-year = l_year.
      IF l_year = c_year.
        EXIT.
      ENDIF.
      l_year = l_year - 1.
    ENDDO.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->STATUSSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_STATUS
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD statusset_get_entityset.
    SELECT * FROM zhrt_ui_leave_st INTO CORRESPONDING FIELDS OF TABLE et_entityset.
    DELETE et_entityset WHERE ident = c_st_05.
    INSERT VALUE #( ident = '99' stnam = 'Все значения' ) INTO et_entityset INDEX 1.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->TIMELINESET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_TIMELINE
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD timelineset_get_entityset.
    DATA: l_reqid TYPE zhrt_ui_kpi_gra-reqid,
          l_uname TYPE uname.
    DATA(lt_keys) = io_tech_request_context->get_source_keys( ).
    TRY.
        l_reqid = lt_keys[ name = 'REQID' ]-value.
        l_uname = lt_keys[ name = 'UNAME' ]-value.
      CATCH cx_root.
        EXIT.
    ENDTRY.
    get_timeline_set( EXPORTING
                        im_reqid = l_reqid
                        im_uname = l_uname
                        io_tech_request_context = io_tech_request_context
                      CHANGING
                        ch_set = et_entityset ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->USERSET_GET_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IO_REQUEST_OBJECT              TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [<---] ER_ENTITY                      TYPE        ZCL_ZHR_KPI_APP_MPC=>TS_USER
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD userset_get_entity.
    DATA: l_uname  TYPE uname,
          l_year   TYPE gjahr,
          l_adm    TYPE char5,
          l_pernr  TYPE persno,
          lt_p0002 TYPE TABLE OF p0002,
          lt_deleg TYPE TABLE OF hrus_d2.
    CONSTANTS: c_rep     TYPE hrus_d2-reppr VALUE 'ZPREM'.
*               c_manager TYPE agr_users-agr_name VALUE 'ZH_FIORI_MANAGER'.
    DATA(lt_keys) = io_tech_request_context->get_keys( ).

    l_uname = VALUE #( lt_keys[ name = 'UNAME' ]-value ).
    l_year = lt_keys[ name = 'YEAR' ]-value.
    l_adm = lt_keys[ name = 'ADMIN' ]-value.


*    DATA(lo_request) = me->mr_request_details.
*    DATA(lt_url_param) = lo_request->t_uri_query_parameter.
*    READ TABLE lt_url_param ASSIGNING FIELD-SYMBOL(<param>) WITH KEY name = 'admin'.
*    IF sy-subrc EQ 0.
*      l_adm = <param>-value.
*    ENDIF.

    IF l_year IS INITIAL.
      l_year = sy-datum+0(4).
    ENDIF.
    er_entity-year = l_year.
    IF l_adm EQ 'false'.
      IF l_uname EQ sy-uname.
        er_entity-uname = l_uname.
      ELSE.
        SELECT us_name FROM hrus_d2
          INTO TABLE lt_deleg
         WHERE us_name = l_uname
           AND rep_name = sy-uname
           AND begda <= sy-datum
           AND endda >= sy-datum
           AND reppr = c_rep
           AND active = abap_true
          .
        IF sy-subrc EQ 0.
          er_entity-uname = l_uname.
        ENDIF.
      ENDIF.
      IF er_entity-uname IS INITIAL.
        DATA: lo_message_container TYPE REF TO /iwbep/if_message_container.
        CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
          RECEIVING
            ro_message_container = lo_message_container.
        CALL METHOD lo_message_container->add_message
          EXPORTING
            iv_msg_type               = /iwbep/cl_cos_logger=>warning
            iv_msg_id                 = 'ZHR_UI5'
*           iv_msg_number             = '002'   " <-- del NaumovSM 14082
            iv_msg_number             = '003'   " <-- ins NaumovSM 14082
            iv_add_to_response_header = abap_true.
*          IV_MSG_TEXT = 'Тестовое сообщение для Бато'.
        CLEAR er_entity.
      ENDIF.
    ELSE.
      IF check_admin( im_uname = sy-uname ) EQ abap_true.
        er_entity-uname = sy-uname.
        er_entity-admin = abap_true.
      ELSE.
        CALL METHOD me->/iwbep/if_mgw_conv_srv_runtime~get_message_container
          RECEIVING
            ro_message_container = lo_message_container.
        CALL METHOD lo_message_container->add_message
          EXPORTING
            iv_msg_type               = /iwbep/cl_cos_logger=>warning
            iv_msg_id                 = 'ZHR_UI5'
*           iv_msg_number             = '002'   " <-- del NaumovSM 14082
            iv_msg_number             = '003'   " <-- ins NaumovSM 14082
            iv_add_to_response_header = abap_true.
*          IV_MSG_TEXT = 'Тестовое сообщение для Бато'.
        CLEAR er_entity.
      ENDIF.
    ENDIF.
    IF er_entity IS NOT INITIAL.
      CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
        EXPORTING
          user_name               = er_entity-uname
        IMPORTING
          employeenumber          = l_pernr
        EXCEPTIONS
          no_employeenumber_found = 1
          subtype_not_available   = 2
          OTHERS                  = 3.
      m_read_inf l_pernr '0002' lt_p0002 sy-datum.
      IF lt_p0002[] IS NOT INITIAL.
        er_entity-fio = |{ lt_p0002[ 1 ]-nachn } { lt_p0002[ 1 ]-vorna } { lt_p0002[ 1 ]-midnm }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->WRITE_APPROVERSET
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CH_ENTITY                      TYPE        ZCL_ZHR_KPI_APP_MPC=>TS_USER
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD write_approverset.
    DATA: lt_approvers_old TYPE TABLE OF zhrt_ui_approver,
          lt_approvers_new LIKE lt_approvers_old,
          lt_orgehs        TYPE TABLE OF orgeh,
          l_approver       TYPE uname,
          pernr            TYPE persno,
          orgeh            TYPE orgeh,
          lt_p0001         TYPE TABLE OF p0001.
    DEFINE m_clear_cash.
      TRY.
        CALL FUNCTION 'ZHR_LP_CLEAR_CACHE_CATALOG'
                      EXPORTING
                        iv_uname       = CONV uname( &1 ).
      CATCH cx_root.
      ENDTRY.
    END-OF-DEFINITION.

    l_approver = VALUE #( ch_entity-toapprovers[ 1 ]-delegate OPTIONAL ).
    IF l_approver IS INITIAL.
      l_approver = sy-uname.
    ENDIF.
    DATA(lv_year) = VALUE #( ch_entity-toapprovers[ 1 ]-year OPTIONAL ).

    CALL FUNCTION 'CRIF_GET_EMPLOYEE_FOR_USER'
      EXPORTING
        user_name               = l_approver
      IMPORTING
        employeenumber          = pernr
      EXCEPTIONS
        no_employeenumber_found = 1
        subtype_not_available   = 2
        OTHERS                  = 3.


*    m_read_inf pernr '0001' lt_p0001.
    m_read_inf pernr '0001' lt_p0001 sy-datum.
    TRY.
        orgeh = get_oe( im_plans = lt_p0001[ 1 ]-plans ).
      CATCH cx_root.
        EXIT.
    ENDTRY.

    SELECT * FROM zhrt_ui_approver INTO TABLE lt_approvers_old WHERE approver = l_approver AND year_appr = lv_year.
    LOOP AT ch_entity-toapprovers ASSIGNING FIELD-SYMBOL(<appr>).
      SPLIT <appr>-orgeh AT ',' INTO TABLE lt_orgehs.
      IF lt_orgehs[] IS NOT INITIAL.
        LOOP AT lt_orgehs ASSIGNING FIELD-SYMBOL(<orgeh>).
          APPEND INITIAL LINE TO lt_approvers_new ASSIGNING FIELD-SYMBOL(<line>).
          MOVE-CORRESPONDING <appr> TO <line>.
          <line>-approver = l_approver.
          <line>-add_approver = <appr>-uname.
          <line>-orgeh = <orgeh>.
          <line>-year_appr = <appr>-year.
          <line>-mandt = sy-mandt.
        ENDLOOP.
      ELSE.
        APPEND INITIAL LINE TO lt_approvers_new ASSIGNING <line>.
        MOVE-CORRESPONDING <appr> TO <line>.
        <line>-approver = l_approver.
        <line>-add_approver = <appr>-uname.
        <line>-year_appr = <appr>-year.
        <line>-mandt = sy-mandt.
      ENDIF.
      CHECK <line>-approver EQ <line>-add_approver.
*      DELETE TABLE lt_approvers_new FROM <line>.
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM lt_approvers_new COMPARING orgeh approver add_approver year_appr queue.

    LOOP AT lt_approvers_old ASSIGNING <line>.
      READ TABLE lt_approvers_new FROM <line> TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        m_clear_cash <line>-add_approver. " Чистим кэш для удаляемого
        DELETE zhrt_ui_approver FROM <line>. "!!!Удаляем из БД
      ELSE.
        DELETE TABLE lt_approvers_new FROM <line>.
        DELETE TABLE lt_approvers_old FROM <line>.
        CONTINUE.
      ENDIF.
    ENDLOOP.

    MODIFY zhrt_ui_approver FROM TABLE lt_approvers_new.
    IF sy-subrc EQ 0.
      COMMIT WORK.
* Чистим кэш для добавленных
      LOOP AT lt_approvers_new ASSIGNING <line> GROUP BY <line>-add_approver.
        LOOP AT GROUP <line> ASSIGNING FIELD-SYMBOL(<line2>).
          m_clear_cash <line2>-add_approver.
          EXIT.
        ENDLOOP.
      ENDLOOP.

      change_table_gra( EXPORTING
                          im_opera = 'ADD'
                          im_orgeh_lead = orgeh
                         CHANGING
                          ch_tab = lt_approvers_new ).
      change_table_gra( EXPORTING
                          im_opera = 'DEL'
                          im_orgeh_lead = orgeh
                         CHANGING
                          ch_tab = lt_approvers_old ).
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ZHR_KPI_APP_DPC_EXT->WRITE_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [<---] TEXT_ID                        TYPE        THEAD-TDNAME
* | [<---] TEXT                           TYPE        ZHRE_UI_COMMENT_LONG
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD write_text.
    DATA: lv_thead TYPE thead,
          lt_text  TYPE TABLE OF pru_char255,
          it_lines TYPE tttext,
          l_text   TYPE string.
    CALL FUNCTION 'INIT_TEXT'
      EXPORTING
        id       = 'ZKPI'
        language = sy-langu
        name     = text_id
        object   = 'Z_FIO_KPI'
      IMPORTING
        header   = lv_thead
      TABLES
        lines    = it_lines[]
      EXCEPTIONS
        id       = 1
        language = 2
        name     = 3
        object   = 4
        OTHERS   = 5.
    l_text = text.
    CALL FUNCTION 'ZHR_SLPIT_STRING'
      EXPORTING
        string        = l_text
        length        = 132
        use_separator = abap_true
      TABLES
        split         = lt_text.
    LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<text_line>).
      APPEND VALUE #( tdline = <text_line>-char255 ) TO it_lines.
    ENDLOOP.
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        client          = sy-mandt
        header          = lv_thead
        savemode_direct = abap_true
      TABLES
        lines           = it_lines[]
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    CHECK sy-subrc = 0.
    CALL FUNCTION 'COMMIT_TEXT'.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZHR_KPI_APP_DPC_EXT->YEARSSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZHR_KPI_APP_MPC=>TT_YEARS
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  method YEARSSET_GET_ENTITYSET.
    DATA: l_uname TYPE uname.
    DATA(lt_keys) = io_tech_request_context->get_source_keys( ).
    TRY.
        l_uname = lt_keys[ name = 'UNAME' ]-value.
      CATCH cx_root.
        EXIT.
    ENDTRY.
    get_years_set( EXPORTING
                     im_uname = l_uname
                     io_tech_request_context = io_tech_request_context
                   CHANGING
                     ch_set = et_entityset ).
  endmethod.
ENDCLASS.