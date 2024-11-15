


  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_CPAT_MARKUP_DEV_BASIS" AS

  /* TODO enter package declarations (types, exceptions, methods etc) here */
 FUNCTION fn_get_dev_cost_calc_markup1(
      prime_deviated_cost_type IN VARCHAR2,
      prime_markup_indicator   IN VARCHAR2,
      price_uom                IN VARCHAR2,
      dev_cost_margin          IN NUMBER,
      NET_WT_SHIPPED           IN NUMBER,
      UNFM_QTY_SHPD            IN NUMBER,
      DEV_COST                 IN NUMBER,
      inv_freight              IN NUMBER,
      dev_cost_allowance       IN NUMBER,
      vendor_inv_cost          IN NUMBER)
   RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE;

   FUNCTION fn_get_dev_cost_calc_sellprice(
      prime_deviated_cost_type IN VARCHAR2,
      prime_cont_ea_mkup_type   IN VARCHAR2,
      price_uom                IN VARCHAR2,
      sales_uom                IN VARCHAR2,
      each_conv_fctr           IN NUMBER,
      DEV_COST                 IN NUMBER,
      inv_freight              IN NUMBER,
      dev_cost_allowance       IN NUMBER,
      vendor_inv_cost          IN NUMBER,
      dev_cost_calc_markup     IN NUMBER,
      prime_min_markup_amount  IN NUMBER,
      each_shpd                IN NUMBER)
   RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE;

END PKG_CPAT_MARKUP_DEV_BASIS;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_CPAT_MARKUP_DEV_BASIS"
AS

FUNCTION fn_get_dev_cost_calc_markup1(
    prime_deviated_cost_type IN VARCHAR2,
    prime_markup_indicator   IN VARCHAR2,
    price_uom                IN VARCHAR2,
    dev_cost_margin          IN NUMBER,
    NET_WT_SHIPPED           IN NUMBER,
    UNFM_QTY_SHPD            IN NUMBER,
    DEV_COST                 IN NUMBER,
    inv_freight              IN NUMBER,
    dev_cost_allowance       IN NUMBER,
    vendor_inv_cost          IN NUMBER)
  RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE
IS
  /***********************************************************************
  * Name: fn_get_dev_cost_calc_markup1
  * Type: Function
  * Description: This function is called by CPAT Markup on Dev Cost Basis.
  *              This report is in Merlin BBT subject area, in the rpd
  *              "Dev Cost Calculated Markup1" column calls this function.
  * Revisions:
  *  Ver        Date        Author           Description
  *  ---------  ----------  ---------------  -----------------------------
  *  1.0        09/17/2014  Jawad Majeed     Created this function
  ************************************************************************/
  dev_cost_calc_markup1 NUMBER;
BEGIN
  dev_cost_calc_markup1 :=
  CASE
  WHEN prime_deviated_cost_type IN ('FX', 'G') THEN
    0
  WHEN prime_deviated_cost_type <> 'FX' OR prime_deviated_cost_type IS NULL THEN
    CASE
    WHEN prime_markup_indicator IN ('D', 'N') OR prime_markup_indicator IS NULL THEN
      CASE
      WHEN price_uom = 'LB' THEN
        CASE WHEN UNFM_QTY_SHPD = 0 THEN 0 ELSE
          dev_cost_margin / ( NET_WT_SHIPPED / UNFM_QTY_SHPD )
        END
      WHEN price_uom   IN ('CS', 'EA') THEN
        dev_cost_margin
      END
    WHEN prime_markup_indicator = '#' THEN
      CASE
      WHEN price_uom    IN ('CS', 'EA') THEN
        CASE WHEN UNFM_QTY_SHPD = 0 THEN 0 ELSE
          ( NET_WT_SHIPPED / UNFM_QTY_SHPD ) * dev_cost_margin
        END
      WHEN price_uom = 'LB' THEN
        dev_cost_margin
      END
    WHEN prime_markup_indicator = 'P' THEN
      CASE
      WHEN prime_deviated_cost_type = 'AL' THEN
        (vendor_inv_cost + dev_cost_allowance ) * dev_cost_margin
      WHEN prime_deviated_cost_type = 'D' THEN
        DEV_COST * dev_cost_margin
      WHEN prime_deviated_cost_type = 'F' THEN
        (DEV_COST + inv_freight ) * dev_cost_margin
      WHEN prime_deviated_cost_type IS NULL THEN
        vendor_inv_cost * dev_cost_margin
      END
    END
  END;
  RETURN(NVL(dev_cost_calc_markup1,0));
END;

FUNCTION fn_get_dev_cost_calc_sellprice(
    prime_deviated_cost_type IN VARCHAR2,
    prime_cont_ea_mkup_type  IN VARCHAR2,
    price_uom                IN VARCHAR2,
    sales_uom                IN VARCHAR2,
    each_conv_fctr           IN NUMBER,
    DEV_COST                 IN NUMBER,
    inv_freight              IN NUMBER,
    dev_cost_allowance       IN NUMBER,
    vendor_inv_cost          IN NUMBER,
    dev_cost_calc_markup     IN NUMBER,
    prime_min_markup_amount  IN NUMBER,
    each_shpd                IN NUMBER)
  RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE
IS
  /***********************************************************************
  * Name: fn_get_dev_cost_calc_sellprice
  * Type: Function
  * Description: This function is called by CPAT Markup on Dev Cost Basis.
  *              This report is in Merlin BBT subject area, in the rpd
  *              "Dev Cost Calculated Sell Price" column calls this function.
  * Revisions:
  *  Ver        Date        Author           Description
  *  ---------  ----------  ---------------  -----------------------------
  *  1.0        09/17/2014  Jawad Majeed     Created this function
  ************************************************************************/
  dev_cost_calc_sellprice NUMBER;
BEGIN
  dev_cost_calc_sellprice :=
  CASE
  WHEN each_shpd = 0 THEN
    CASE
    WHEN prime_deviated_cost_type = 'AL' THEN
      vendor_inv_cost              + dev_cost_allowance + dev_cost_calc_markup
    WHEN prime_deviated_cost_type IN ('D', 'FX', 'G') THEN
      dev_cost                     + dev_cost_calc_markup
    WHEN prime_deviated_cost_type = 'F' THEN
      dev_cost+ inv_freight + dev_cost_calc_markup
    WHEN prime_deviated_cost_type IS NULL THEN
      vendor_inv_cost+ dev_cost_calc_markup
    END
  ELSE
    CASE
    WHEN prime_cont_ea_mkup_type = 'P' THEN
      CASE
      WHEN prime_deviated_cost_type = 'AL' THEN
        CASE
        WHEN sales_uom = 'CS' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'EA' THEN
          CASE
          WHEN each_conv_fctr = 0 THEN
            0
          ELSE
            (((vendor_inv_cost+ dev_cost_allowance + dev_cost_calc_markup) / each_conv_fctr) * (1 + (prime_min_markup_amount / 100)))
          END
        WHEN sales_uom = 'CS' AND price_uom = 'LB' OR sales_uom = 'CS' AND price_uom = 'EA' OR sales_uom = 'EA' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'LB' THEN
          ((vendor_inv_cost+ dev_cost_allowance + dev_cost_calc_markup) * (1 + (prime_min_markup_amount / 100)))
        END
      WHEN prime_deviated_cost_type IN ('D', 'FX') THEN
        CASE
        WHEN sales_uom = 'CS' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'EA' THEN
          CASE WHEN each_conv_fctr = 0 THEN 0 ELSE
            (((dev_cost+ dev_cost_calc_markup) / each_conv_fctr) * (1 + (prime_min_markup_amount / 100)))
          END
        WHEN sales_uom = 'CS' AND price_uom = 'LB' OR sales_uom = 'CS' AND price_uom = 'EA' OR sales_uom = 'EA' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'LB' THEN
          ((dev_cost+ dev_cost_calc_markup) * (1 + (prime_min_markup_amount / 100)))
        END
      WHEN prime_deviated_cost_type = 'F' THEN
        CASE
        WHEN sales_uom = 'CS' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'EA' THEN
          CASE
          WHEN each_conv_fctr = 0 THEN
            0
          ELSE
            (((dev_cost+ inv_freight + dev_cost_calc_markup) / each_conv_fctr) * (1 + prime_min_markup_amount / 100))
          END
        WHEN sales_uom = 'CS' AND price_uom = 'LB' OR sales_uom = 'CS' AND price_uom = 'EA' OR sales_uom = 'EA' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'LB' THEN
          (dev_cost+ dev_cost_calc_markup) * (1 + prime_min_markup_amount / 100)
        END
      WHEN prime_deviated_cost_type IS NULL THEN
        CASE
        WHEN sales_uom = 'CS' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'EA' THEN
          CASE
          WHEN each_conv_fctr = 0 THEN
            0
          ELSE
            (((vendor_inv_cost+ dev_cost_calc_markup) / each_conv_fctr) * (1 +( prime_min_markup_amount / 100)))
          END
        WHEN sales_uom = 'CS' AND price_uom = 'LB' OR sales_uom = 'CS' AND price_uom = 'EA' OR sales_uom = 'EA' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'LB' THEN
          ((vendor_inv_cost+ dev_cost_calc_markup) * (1 + (prime_min_markup_amount / 100)))
        END
      END
    WHEN prime_cont_ea_mkup_type <> 'P' OR prime_cont_ea_mkup_type IS NULL THEN
      CASE
      WHEN prime_deviated_cost_type = 'AL' THEN
        CASE
        WHEN sales_uom = 'CS' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'EA' THEN
          CASE
          WHEN each_conv_fctr = 0 THEN
            prime_min_markup_amount
          ELSE
            (((vendor_inv_cost+ dev_cost_allowance + dev_cost_calc_markup) / each_conv_fctr) + prime_min_markup_amount)
          END
        WHEN sales_uom = 'CS' AND price_uom = 'LB' OR sales_uom = 'CS' AND price_uom = 'EA' OR sales_uom = 'EA' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'LB' THEN
          ((vendor_inv_cost+ dev_cost_allowance + dev_cost_calc_markup) + prime_min_markup_amount)
        END
      WHEN prime_deviated_cost_type IN ('D', 'FX') THEN
        CASE
        WHEN sales_uom = 'CS' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'EA' THEN
          CASE
          WHEN each_conv_fctr = 0 THEN
            prime_min_markup_amount
          ELSE
            ((dev_cost+ dev_cost_calc_markup) / each_conv_fctr) + prime_min_markup_amount
          END
        WHEN sales_uom = 'CS' AND price_uom = 'LB' OR sales_uom = 'CS' AND price_uom = 'EA' OR sales_uom = 'EA' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'LB' THEN
          dev_cost+ dev_cost_calc_markup + prime_min_markup_amount
        END
      WHEN prime_deviated_cost_type = 'F' THEN
        CASE
        WHEN sales_uom = 'CS' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'EA' THEN
          CASE
          WHEN each_conv_fctr = 0 THEN
            prime_min_markup_amount
          ELSE
            ( ((dev_cost+ inv_freight + dev_cost_calc_markup) / each_conv_fctr) + prime_min_markup_amount)
          END
        WHEN sales_uom = 'CS' AND price_uom = 'LB' OR sales_uom = 'CS' AND price_uom = 'EA' OR sales_uom = 'EA' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'LB' THEN
          dev_cost+ inv_freight + dev_cost_calc_markup + prime_min_markup_amount
        END
      WHEN prime_deviated_cost_type IS NULL THEN
        CASE
        WHEN sales_uom = 'CS' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'EA' THEN
          CASE
          WHEN each_conv_fctr = 0 THEN
            prime_min_markup_amount
          ELSE
            (( (vendor_inv_cost+ dev_cost_calc_markup) / each_conv_fctr) + prime_min_markup_amount)
          END
        WHEN sales_uom = 'CS' AND price_uom = 'LB' OR sales_uom = 'CS' AND price_uom = 'EA' OR sales_uom = 'EA' AND price_uom = 'CS' OR sales_uom = 'EA' AND price_uom = 'LB' THEN
          vendor_inv_cost+ dev_cost_calc_markup + prime_min_markup_amount
        END
      END
    END
  END ;
  RETURN(NVL(dev_cost_calc_sellprice,0));
END;

END PKG_CPAT_MARKUP_DEV_BASIS;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_ICM_AUDIT_YRLY"
IS
  FUNCTION get_audit_yrl_by_rng (   pi_start_dt             DATE,
                                    pi_end_dt               DATE,
                                    pi_fisc_yr_wk_fltr      NUMBER,
                                    po_cnt            OUT   INTEGER,
                                    po_main_header    OUT   VARCHAR2,
                                    po_audit_data     OUT   SYS_REFCURSOR,
                                    po_err_msg        OUT   VARCHAR2
                                  )
  RETURN INTEGER;

  FUNCTION get_audit_yrly  ( pi_odate                  DATE,
                             po_start_dt         OUT   DATE,
                             po_end_dt           OUT   DATE,
                             po_cnt              OUT   INTEGER,
                             po_main_header      OUT   VARCHAR2,
                             po_audit_data       OUT   SYS_REFCURSOR,
                             po_err_msg          OUT   VARCHAR2
                           )
  RETURN INTEGER;

END pkg_icm_audit_yrly;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_ICM_AUDIT_YRLY"
IS
  FUNCTION get_audit_yrl_by_rng (   pi_start_dt             DATE,
                                    pi_end_dt               DATE,
                                    pi_fisc_yr_wk_fltr      NUMBER,
                                    po_cnt            OUT   INTEGER,
                                    po_main_header    OUT   VARCHAR2,
                                    po_audit_data     OUT   SYS_REFCURSOR,
                                    po_err_msg        OUT   VARCHAR2
                                  )
  RETURN INTEGER
  IS

    v_xiw_obj     ICM_REC_LST := ICM_REC_LST();
  BEGIN


   /* Fetching 2+current years of data from SALES_CORP   */
    BEGIN
     SELECT icm_rec(
           s.div_nbr,
           t.FISC_YR_WK,
           /* regular sales facts */
           SUM(
                CASE WHEN (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc <> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                     THEN s.EACH_SHIP
                     ELSE 0
                END
            ) ,--EACH_SHIP,
            SUM(
                CASE WHEN (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.PRIME_MIN_MRK_AMT
                      ELSE 0
                END
            ) ,--PRIME_MIN_MRK_AMT,
            SUM(
                CASE WHEN  (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.REP_COST_EXTND
                      ELSE 0
                END
            ),--REP_COST_EXTND,
            /*
            SUM(
                CASE WHEN s.trans_typ not in ('AP','AN')
                      AND s.prod_nbr NOT LIKE 'NPAC%'
                      AND s.prod_nbr <> '99999999999'
                      AND (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.ACTL_GP
                      ELSE 0
                END
            ) ,--ACTL_GP,*/
            SUM(
                CASE WHEN (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.DVT_COST_EXTND
                      ELSE 0
                END
            ),--DVT_COST_EXTND,
            SUM(
                CASE WHEN  (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.DWA_COST_EXTND
                      ELSE 0
                END
            ),--DWA_COST_EXTND,
            SUM(
                CASE WHEN (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN Case when t.FISC_YR_WK <= pi_fisc_yr_wk_fltr then s.GRS_SLS_EXTND
                                when t.FISC_YR_WK > pi_fisc_yr_wk_fltr
                                        and d.div_typ_cd in ( Select PARAM_VALUE_CHR from xdmadm.param_value where app_name = 'ICM' and param_type = 'SLS_XTRCT_ASC_EXCLU' )
                                    then s.grs_sls_extnd
                                when t.FISC_YR_WK > pi_fisc_yr_wk_fltr
                                        and nvl(s.ASC_ORD_APLY_IND, 'N') = 'P'
                                    then s.GRS_SLS_EXTND_BFOR_ASC
                                else
                                    nvl(s.GRS_SLS_EXTND,0)
                                end

                      ELSE 0
                END
            ),--GRS_SLS_EXTND,
            SUM(
                CASE WHEN (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.NET_WT_SHIP
                      ELSE 0
                END
            ) ,--NET_WT_SHIP,
            SUM(
                CASE WHEN (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.PRIME_MRK_AMT
                      ELSE 0
                END
            ),--PRIME_MRK_AMT,
            SUM(
                CASE WHEN  (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.SRCHRG
                      ELSE 0
                END
            ),--SRCHRG,
            SUM(
                CASE WHEN  (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.QTY_SHIP
                      ELSE 0
                END
            ),--QTY_SHIP,
            SUM(
                CASE WHEN  (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.ORIG_MRGN_BSIS_AMT_EXTND
                      ELSE 0
                END
            ),--ORIG_MRGN_BSIS_AMT_EXTND,
            SUM(
                CASE WHEN  (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.UNFRM_QTY_SHIP
                      ELSE 0
                END
            ),--UNFRM_QTY_SHIP,
            SUM(
                CASE WHEN (s.UNFRM_QTY_SHIP <> 0 or s.GRS_SLS_EXTND <> 0 or s.each_prc<> 0 or s.srchrg <> 0 or s.qty_ship <> 0 or s.bsis_amt <> 0)
                      THEN s.UNIT_SHIP
                      ELSE 0
                END
            ),--UNIT_SHIP,
            /* AP facts */
            SUM(
                  CASE WHEN s.trans_typ = 'AP'
                        THEN s.ttl_alwnc
                        ELSE 0
                  END
                ),-- ttl_alwnc,
            SUM(
                  CASE WHEN s.trans_typ = 'AP'
                        THEN s.ttl_dvt
                        ELSE 0
                  END
                ),--ttl_dvt,
             /* AC facts */
             SUM(
                  CASE WHEN s.trans_typ not in ('AN', 'AP')
                        THEN s.PROD_ALW
                        ELSE 0
                  END
                ),--PROD_ALW,
             SUM(
                  CASE WHEN s.trans_typ not in ('AN', 'AP')
                       THEN s.PROD_CHRG
                       ELSE 0
                  END
                ),--PROD_CHRG,
              /* NPAC facts */
            SUM(
                  CASE  WHEN s.trans_typ = 'AN'
                        THEN s.non_prod_alwnc
                        ELSE 0
                  END
                ),--non_prod_alwnc,
             SUM(
                  CASE WHEN s.trans_typ = 'AN'
                       THEN s.non_prod_chrg
                       ELSE 0
                  END
                ), --non_prod_chrg
            /* rbt facts */
             SUM(rbt_loc_accrl_amt)  ,
             SUM(rbt_natl_accrl_amt) ,
             SUM(rbt_loc_exp_amt)    ,
             SUM(rbt_natl_exp_amt))
      BULK COLLECT INTO v_xiw_obj
      FROM xdmadm.sales_corp s INNER JOIN xdmadm.div_corp d ON (s.div_nbr = d.div_nbr)
     INNER JOIN xdmadm.time_corp t ON(s.prcs_dt = t.clndr_dt)
     WHERE s.prcs_dt between pi_start_dt and pi_end_dt
       AND s.xfer_from_dt IS NULL
       AND d.conv_to_div_nbr IS NULL
       AND d.prcs_sys <> 'PSYS'
       AND (d.inact_dt IS NULL OR d.inact_dt >=  TO_DATE('20110101','yyyymmdd'))
     GROUP BY s.div_nbr,
              t.FISC_YR_WK;
    EXCEPTION
      WHEN OTHERS THEN
        po_err_msg := 'Data fetch from tables failed '||SUBSTR(SQLERRM,1,1950);
        ROLLBACK;
        RETURN -1;
    END;

    po_main_header := 'DIV_NBR'||CHR(9)||
                      'FISC_YR_WK'||CHR(9)||
                      'EACH_SHIP'||CHR(9)||
                      'PRIME_MIN_MRK_AMT'||CHR(9)||
                      'REP_COST_EXTND'||CHR(9)||
                      'DVT_COST_EXTND'||CHR(9)||
                      'DWA_COST_EXTND'||CHR(9)||
                      'GRS_SLS_EXTND'||CHR(9)||
                      'NET_WT_SHIP'||CHR(9)||
                      'PRIME_MRK_AMT'||CHR(9)||
                      'SRCHRG'||CHR(9)||
                      'QTY_SHIP'||CHR(9)||
                      'ORIG_MRGN_BSIS_AMT_EXTND'||CHR(9)||
                      'UNFRM_QTY_SHIP'||CHR(9)||
                      'UNIT_SHIP'||CHR(9)||
                      'TTL_ALWNC'||CHR(9)||
                      'TTL_DVT'||CHR(9)||
                      'PROD_ALW'||CHR(9)||
                      'PROD_CHRG'||CHR(9)||
                      'NON_PROD_ALWNC'||CHR(9)||
                      'NON_PROD_CHRG'||CHR(9)||
                      'RBT_LOC_ACCRL_AMT'||CHR(9)||
                      'RBT_NATL_ACCRL_AMT'||CHR(9)||
                      'RBT_LOC_EXP_AMT'||CHR(9)||
                      'RBT_NATL_EXP_AMT';

    OPEN po_audit_data FOR SELECT DIV_NBR||CHR(9)||
                                  FISC_YR_WK||CHR(9)||
                                  EACH_SHIP||CHR(9)||
                                  PRIME_MIN_MRK_AMT||CHR(9)||
                                  REP_COST_EXTND||CHR(9)||
                                  DVT_COST_EXTND||CHR(9)||
                                  DWA_COST_EXTND||CHR(9)||
                                  GRS_SLS_EXTND||CHR(9)||
                                  NET_WT_SHIP||CHR(9)||
                                  PRIME_MRK_AMT||CHR(9)||
                                  SRCHRG||CHR(9)||
                                  QTY_SHIP||CHR(9)||
                                  ORIG_MRGN_BSIS_AMT_EXTND||CHR(9)||
                                  UNFRM_QTY_SHIP||CHR(9)||
                                  UNIT_SHIP||CHR(9)||
                                  TTL_ALWNC||CHR(9)||
                                  TTL_DVT||CHR(9)||
                                  PROD_ALW||CHR(9)||
                                  PROD_CHRG||CHR(9)||
                                  NON_PROD_ALWNC||CHR(9)||
                                  NON_PROD_CHRG||CHR(9)||
                                  RBT_LOC_ACCRL_AMT||CHR(9)||
                                  RBT_NATL_ACCRL_AMT||CHR(9)||
                                  RBT_LOC_EXP_AMT||CHR(9)||
                                  RBT_NATL_EXP_AMT
                             FROM TABLE(CAST(v_xiw_obj AS ICM_REC_LST));

    po_cnt := v_xiw_obj.COUNT();
    po_err_msg := NULL;

    RETURN 0;
  EXCEPTION
    WHEN OTHERS THEN
      po_err_msg := 'Main Exception '||SUBSTR(SQLERRM,1,1950);
      ROLLBACK;
      RETURN -1;
  END get_audit_yrl_by_rng;

  FUNCTION get_audit_yrly  ( pi_odate                  DATE,
                             po_start_dt         OUT   DATE,
                             po_end_dt           OUT   DATE,
                             po_cnt              OUT   INTEGER,
                             po_main_header      OUT   VARCHAR2,
                             po_audit_data       OUT   SYS_REFCURSOR,
                             po_err_msg          OUT   VARCHAR2
                           )
  RETURN INTEGER
  IS

    v_start_dt          DATE;
    v_end_dt            DATE;
    v_ret               INTEGER;
    l_fisc_wk_of_yr     NUMBER;
    l_fisc_yr           NUMBER;
    l_fisc_wk_of_yr_pr  NUMBER;
    v_fisc_yr_wk_fltr   NUMBER;

  BEGIN


    BEGIN
     SELECT fisc_wk_of_yr, fisc_yr
       INTO l_fisc_wk_of_yr, l_fisc_yr
       FROM xdmadm.time_corp WHERE clndr_dt = pi_odate;
    EXCEPTION
      WHEN OTHERS THEN
        po_err_msg := 'Failed with selecting from time_corp '||SUBSTR(SQLERRM,1,1950);
        ROLLBACK;
        RETURN -1;
    END;

    BEGIN
     SELECT PARAM_VALUE_NBR
        INTO v_fisc_yr_wk_fltr
        from xdmadm.param_value
        WHERE APP_NAME = 'ICM' AND PARAM_TYPE = 'YRL_AUDIT_ASC_TM_FLTR' AND PARAM_CODE = 'FISC_YR_WK1'
;
    EXCEPTION
      WHEN OTHERS THEN
        po_err_msg := 'Missing FISC_YR_WK Fileter in Param table  '||SUBSTR(SQLERRM,1,1950);
        ROLLBACK;
        RETURN -1;
    END;

    l_fisc_wk_of_yr_pr := l_fisc_wk_of_yr - 1;

    IF l_fisc_wk_of_yr_pr = 0 THEN
      BEGIN
        SELECT MAX(clndr_dt)
          INTO v_end_dt
          FROM xdmadm.time_corp
         WHERE fisc_yr = l_fisc_yr - 1;
      EXCEPTION
        WHEN OTHERS THEN
          po_err_msg := 'Failed with selecting from time_corp '||SUBSTR(SQLERRM,1,1950);
          ROLLBACK;
          RETURN -1;
      END;

    ELSE

      BEGIN
        SELECT MAX(clndr_dt)
          INTO v_end_dt
          FROM xdmadm.time_corp
         WHERE fisc_yr = l_fisc_yr
           AND fisc_wk_of_yr = l_fisc_wk_of_yr_pr;
      EXCEPTION
          WHEN OTHERS THEN
            po_err_msg := 'Failed with selecting from time_corp '||SUBSTR(SQLERRM,1,1950);
            ROLLBACK;
            RETURN -1;
      END;

    END IF;

    v_start_dt := TO_DATE(TO_CHAR(EXTRACT(YEAR FROM pi_odate)-2,'0000')||TO_CHAR(EXTRACT(MONTH FROM pi_odate),'00')||TO_CHAR(EXTRACT(DAY FROM pi_odate),'00'),'YYYYMMDD');

    v_ret := pkg_icm_audit_yrly.get_audit_yrl_by_rng(
                                                      pi_start_dt             =>      v_start_dt,
                                                      pi_end_dt               =>      v_end_dt,
                                                      pi_fisc_yr_wk_fltr      =>      v_fisc_yr_wk_fltr,
                                                      po_cnt                  =>      po_cnt,
                                                      po_main_header          =>      po_main_header,
                                                      po_audit_data           =>      po_audit_data,
                                                      po_err_msg              =>      po_err_msg
                                                   );

     IF v_ret <> 0 THEN

        RETURN -1;

     END IF;

     po_start_dt := v_start_dt;
     po_end_dt   := v_end_dt;

     RETURN 0;

  EXCEPTION
    WHEN OTHERS THEN
      po_err_msg := 'Main Exception '||SUBSTR(SQLERRM,1,1950);
      ROLLBACK;
      RETURN -1;
  END get_audit_yrly;
END pkg_icm_audit_yrly;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_CPAT_MARGIN_PRICE_BASIS" AS

  /* TODO enter package declarations (types, exceptions, methods etc) here */
  FUNCTION          get_full_cs_margin1(prime_deviated_cost_type IN VARCHAR2,
                                      prime_markup_indicator IN VARCHAR2,
                                      price_uom IN VARCHAR2,
                                      sales_uom IN VARCHAR2,
                                      vendr_inv_cost IN NUMBER,
                                      margin IN NUMBER,
                                      NET_WT_SHIPPED In NUMBER,
                                      UNFM_QTY_SHPD IN NUMBER
                                                                                          )
   RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE;

   FUNCTION          get_full_cs_sellprice(prime_deviated_cost_type IN VARCHAR2,
                                        vendor_inv_cost IN NUMBER,
                                        allowance IN NUMBER,
                                        calc_full_cs_margin IN NUMBER,
                                        inv_freight IN NUMBER,
                                        DEV_COST IN NUMBER
                                        )
   RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE;

  FUNCTION          get_calculated_each_csmargin(prime_deviated_cost_type IN VARCHAR2,
                                              prime_each_markup_type IN VARCHAR2,
                                              each_shipped IN NUMBER,
                                              price_uom IN VARCHAR2,
                                              sales_uom IN VARCHAR2,
                                              calculated_full_cs_sell_price IN NUMBER,
                                              prime_contract_ea_markupamount IN NUMBER,
                                              each_conv_factor IN NUMBER
                                             )
   RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE;

END PKG_CPAT_MARGIN_PRICE_BASIS;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_CPAT_MARGIN_PRICE_BASIS"
AS
FUNCTION get_full_cs_margin1(
    prime_deviated_cost_type IN VARCHAR2,
    prime_markup_indicator   IN VARCHAR2,
    price_uom                IN VARCHAR2,
    sales_uom                IN VARCHAR2,
    vendr_inv_cost           IN NUMBER,
    margin                   IN NUMBER,
    NET_WT_SHIPPED           IN NUMBER,
    UNFM_QTY_SHPD            IN NUMBER )
  RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE
IS
  /***********************************************************************
  * Name: get_full_cs_margin1
  * Type: Function
  * Description: This function is called by CPAT Margin on Price Basis.
  *              This report is in Merlin BBT subject area, in the rpd
  *              "Caclculated Full CS Margin1" column calls this function.
  * Revisions:
  *  Ver        Date        Author           Description
  *  ---------  ----------  ---------------  -----------------------------
  *  1.0        09/17/2014  Jawad Majeed     Created this function
  ************************************************************************/
  calculated_full_cs_margin1 NUMBER;
BEGIN
  calculated_full_cs_margin1 :=
  CASE
  WHEN prime_deviated_cost_type = 'FX' THEN
    0
  ELSE
    CASE
    WHEN prime_markup_indicator = 'P' THEN
      CASE
      WHEN sales_uom = 'EA' AND price_uom = 'EA' THEN
        ((vendr_inv_cost / (1 - (margin / 100))) - vendr_inv_cost)
      WHEN sales_uom = 'CS' AND price_uom = 'CS' THEN
        ((vendr_inv_cost / (1 - (margin / 100))) - vendr_inv_cost)
      WHEN price_uom = 'LB' THEN
        ((vendr_inv_cost / (1 - (margin / 100))) - vendr_inv_cost)
      WHEN sales_uom = 'EA' AND price_uom = 'CS' THEN
        ((vendr_inv_cost / (1 - (margin / 100))) - vendr_inv_cost)
      END
    ELSE
      CASE
      WHEN price_uom = 'LB' THEN
        margin
      ELSE
        CASE WHEN UNFM_QTY_SHPD = 0 THEN 0
        ELSE
        ((NET_WT_SHIPPED / UNFM_QTY_SHPD) * margin)
        END
      END
    END
  END ;
  RETURN(NVL(calculated_full_cs_margin1,0));
END;

FUNCTION get_full_cs_sellprice(
    prime_deviated_cost_type IN VARCHAR2,
    vendor_inv_cost          IN NUMBER,
    allowance                IN NUMBER,
    calc_full_cs_margin      IN NUMBER,
    inv_freight              IN NUMBER,
    DEV_COST                 IN NUMBER )
  RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE
IS
  /***********************************************************************
  * Name: get_full_cs_sellprice
  * Type: Function
  * Description: This function is called by CPAT Margin on Price Basis.
  *              This report is in Merlin BBT subject area, in the rpd
  *              "Caclculated Full CS Sell Price" column calls this function.
  * Revisions:
  *  Ver        Date        Author           Description
  *  ---------  ----------  ---------------  -----------------------------
  *  1.0        09/17/2014  Jawad Majeed     Created this function
  ************************************************************************/
  calc_full_cs_sell_price NUMBER;
BEGIN
  calc_full_cs_sell_price :=
  CASE
  WHEN prime_deviated_cost_type = 'AL' THEN
    vendor_inv_cost + allowance + calc_full_cs_margin
  WHEN prime_deviated_cost_type = 'D' THEN
    calc_full_cs_margin + DEV_COST
  WHEN prime_deviated_cost_type = 'F' THEN
    DEV_COST + inv_freight + calc_full_cs_margin
  WHEN prime_deviated_cost_type = 'FX' THEN
    DEV_COST
  WHEN prime_deviated_cost_type IS NULL THEN
    vendor_inv_cost + calc_full_cs_margin
  END;
  RETURN(NVL(calc_full_cs_sell_price,0));
END;

FUNCTION get_calculated_each_csmargin(
    prime_deviated_cost_type       IN VARCHAR2,
    prime_each_markup_type         IN VARCHAR2,
    each_shipped                   IN NUMBER,
    price_uom                      IN VARCHAR2,
    sales_uom                      IN VARCHAR2,
    calculated_full_cs_sell_price  IN NUMBER,
    prime_contract_ea_markupamount IN NUMBER,
    each_conv_factor               IN NUMBER )
  RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE
IS
  /***********************************************************************
  * Name: get_calculated_each_csmargin
  * Type: Function
  * Description: This function is called by CPAT Margin on Price Basis.
  *              This report is in Merlin BBT subject area, in the rpd
  *              "Caclculated Each CS Margin" column calls this function.
  * Revisions:
  *  Ver        Date        Author           Description
  *  ---------  ----------  ---------------  -----------------------------
  *  1.0        09/17/2014  Jawad Majeed     Created this function
  ************************************************************************/
  calculated_each_csmargin NUMBER;
BEGIN
  calculated_each_csmargin :=
  CASE
  WHEN each_shipped <> 0 THEN
    CASE
    WHEN prime_deviated_cost_type <> 'FX' OR prime_deviated_cost_type IS NULL THEN
      CASE
      WHEN prime_each_markup_type = 'P' THEN
        CASE
        WHEN sales_uom = 'CS' AND price_uom = 'CS' THEN
          CASE WHEN each_conv_factor = 0 THEN 0 ELSE
            (calculated_full_cs_sell_price / each_conv_factor) * (prime_contract_ea_markupamount / 100)
          END
        WHEN sales_uom = 'EA' AND price_uom = 'EA' THEN
          CASE WHEN each_conv_factor = 0 THEN 0 ELSE
            (calculated_full_cs_sell_price / each_conv_factor) * (prime_contract_ea_markupamount / 100)
          END
        WHEN sales_uom = 'EA' AND price_uom = 'CS' THEN
          CASE WHEN each_conv_factor = 0 THEN 0 ELSE
            (calculated_full_cs_sell_price / each_conv_factor) * (prime_contract_ea_markupamount / 100)
          END
        WHEN price_uom = 'LB' THEN
          (calculated_full_cs_sell_price) * (prime_contract_ea_markupamount / 100)
        END
      ELSE
        prime_contract_ea_markupamount
      END
    END
  ELSE
    0
  END;
  RETURN(NVL(calculated_each_csmargin,0));
END;

END PKG_CPAT_MARGIN_PRICE_BASIS;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_816_867" as

FUNCTION fn_extract_816_pepsi
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
      RETURN NUMBER;

FUNCTION fn_extract_816_ecolab
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
      RETURN NUMBER;
FUNCTION fn_extract_867_pepsi
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
      RETURN NUMBER;

FUNCTION fn_extract_867_ecolab
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
      RETURN NUMBER;

END pkg_816_867;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_816_867" AS
/*********************************************************************
**               Name: pkg_816_867               **
**                                                                  **
** Created  By: Arnie Witt                          Date: 21 Aug 15 **
**                               Ver. 1.0                           **
**********************************************************************
**                              Description                         **
**                                                                  **
**      Extract functionality for pepsi and ecolab                  **
**    This package contains four callable functions:                **
**    1.  fn_extract_816_pepsi :  PEPSI                             **
**    2.  fn_extract_816_ecolab :  ECOLAB                           **
**    3.  fn_extract_867_pepsi :  PEPSI                             **
**    4.  fn_extract_867_ecolab :  ECOLAB                           **
**      all 4 functions write records to table xdmadm.data_816_867  **
**                                                                  **
**  Return:  0 - no errors occurred during processing.              **
**           <> 0 - failure during execution                        **
**                                                                  **
**                                                                  **
**********************************************************************
**  Version               Changes Description                       **
**  =======               ===================                       **
**    1.0       Initial release of this function.                   **
*********************************************************************/
-------------------
-- VARIABLES
-------------------
    g_total_timer_begin         PLS_INTEGER;
    g_timer_end                 PLS_INTEGER;
    g_run_time                  PLS_INTEGER;
    g_message                   VARCHAR2(256);
    g_how_many                  INTEGER;
    g_sqlcode                   NUMBER;
    g_total_lines               INTEGER;
    g_this_week                 INTEGER;
    g_this_year                 INTEGER;
    g_begin_week                DATE;
    g_end_week                  DATE;
    g_records_inserted          INTEGER;
    g_cases                     NUMBER;

   --- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
PROCEDURE sp_calc_run_time
--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
-----------------------------------------
-- calculates the run time
-----------------------------------------
    (
        run_time_i  PLS_INTEGER,
        desc_i      VARCHAR2
    )
    IS
        v_secs NUMBER;
        v_mins NUMBER;
        v_hrs  NUMBER;
BEGIN
        v_secs    := ROUND((run_time_i) / 100, 2);
        v_hrs     := TRUNC(v_secs / 3600);
        v_secs    := v_secs - (v_hrs * 3600);
        v_mins    := TRUNC(v_secs / 60);
        v_secs    := ROUND(v_secs - (v_mins * 60));
        g_message := desc_i  || v_hrs || ' hours; ' || v_mins || ' minutes; ' || v_secs || ' seconds';
        dbms_output.put_line(g_message);

END sp_calc_run_time;
 --- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
PROCEDURE sp_last_week_dates
--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
-----------------------------------------
-- obtain calendar range for last week using ODATE
-----------------------------------------
    (
        odate_i      VARCHAR2
    )
    IS
        v_secs NUMBER;
        v_mins NUMBER;
        v_hrs  NUMBER;
BEGIN
    SELECT  fisc_yr, fisc_wk_of_yr - 1 this_week
    INTO  g_this_year , g_this_week
    FROM xdmadm.time_corp
    WHERE clndr_dt = odate_i;

    IF g_this_week = 0 THEN
        g_this_year := g_this_year - 1;
        SELECT MAX(fisc_wk_of_yr)
        INTO g_this_week
        FROM xdmadm.time_corp
        WHERE fisc_yr = g_this_year;
    END IF;

    SELECT  MIN(clndr_dt),MAX(clndr_dt)
    INTO g_begin_week, g_end_week
    FROM xdmadm.time_corp
    WHERE fisc_yr = g_this_year
    AND fisc_wk_of_yr = g_this_week;

 DBMS_OUTPUT.put_line ('DATE RANGE: ' ||  g_begin_week || ' / ' ||  g_end_week);

    EXCEPTION
        WHEN OTHERS THEN
            g_sqlcode := SQLCODE;
            g_message := 'sp_last_week_dates sqlerror ' || g_sqlcode || ' ' || SUBSTR(SQLERRM, 1, 75);
            DBMS_OUTPUT.put_line(g_message);
            RAISE;
END sp_last_week_dates;
  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
FUNCTION fn_extract_816_pepsi    -- PEPSI
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
 RETURN NUMBER
   IS
  BEGIN
    DBMS_OUTPUT.put_line('BEGIN fn_extract_816_pepsi at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_total_timer_begin := dbms_utility.get_time;
    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'PEPSI816', action_name => 'DELETES');

    DELETE FROM xdmadm.data_816_867
    WHERE data_who = 'PEPSI816' ;

    sp_last_week_dates(p_odate);

    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'PEPSI816', action_name => 'INSERTS');

    INSERT /*+ PARALLEL APPEND */ INTO xdmadm.data_816_867
            (data_who, data_key, data_data)
        SELECT DISTINCT  'PEPSI816'  ,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd data_key,
            'DT1' ||
            RPAD (NVL (LTRIM (TRANSLATE (c.cust_nm,  '*^|><@~`&','    ')),
                'Name blank in db') ,25,' ') ||
            LPAD (NVL (LTRIM (TRANSLATE (c.cust_nbr,  '*^|><@~`&','    ')),
                ' ') ,15, 0) ||
            RPAD (NVL (LTRIM (TRANSLATE (c.addr_ln_1,  '*^|><@~`&','        ')),
                'Address blank in db') ,25,' ') ||
            RPAD (NVL (LTRIM (TRANSLATE (c.city,  '*^|><@~`&','     ')),
                'CIty blank in db') ,18,' ') ||
            RPAD (NVL (LTRIM (TRANSLATE (c.st,  '*^|><@~`&','      ')),
                'XX') ,2,' ') ||
            LPAD (NVL (LTRIM (TRANSLATE (c.zip_cd,  '*^|><@~`&','         ')),
                ' ') ,9, 0)
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
             xdmadm.cust_corp c, xdmadm.pim_corp pim
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.cust_nbr = c.cust_nbr
        AND s.div_nbr = c.div_nbr
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.xfer_from_dt IS NULL
      UNION ALL
         SELECT DISTINCT 'PEPSI816' ,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd data_key,
            'HDR' ||
            RPAD(UPPER(p_partner_id), 15, ' ') ||
            'GEN' ||
            '816' ||
            TO_CHAR (SYSDATE, 'YYYYMMDD') ||
            RPAD(d.div_nm, 25, ' ') ||
            RPAD(d.brnch_cd, 2, ' ')
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
               xdmadm.pim_corp pim
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.xfer_from_dt IS NULL;

    COMMIT;

    SELECT COUNT(1)
    INTO g_how_many
    FROM xdmadm.data_816_867
    WHERE data_who =  'PEPSI816';

    DBMS_OUTPUT.put_line('***************  Number Records created: ' || g_how_many);

    DBMS_OUTPUT.put_line('END fn_extract_816_pepsi at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_timer_end := dbms_utility.get_time;
    g_run_time  := g_timer_end - g_total_timer_begin; -- milliseconds
    sp_calc_run_time(g_run_time, 'TOTAL RUN TIME : ');
    DBMS_APPLICATION_INFO.SET_MODULE(null,null);

    RETURN 0;
    EXCEPTION
        WHEN OTHERS THEN
           DBMS_APPLICATION_INFO.SET_MODULE(null,null);
            g_sqlcode := SQLCODE;
            g_message := 'fn_extract_816_pepsi sqlerror ' || g_sqlcode || ' ' || SUBSTR(SQLERRM, 1, 75);
            DBMS_OUTPUT.put_line(g_message);
            RETURN -2;
    END fn_extract_816_pepsi;

  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
FUNCTION fn_extract_816_ecolab
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
 RETURN NUMBER
   IS
  BEGIN
    DBMS_OUTPUT.put_line('BEGIN fn_extract_816_ecolab at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_total_timer_begin := dbms_utility.get_time;
    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'ECOLAB816', action_name => 'DELETES');

    DELETE FROM xdmadm.data_816_867
    WHERE data_who = 'ECOLAB816' || p_mfr_num;

    sp_last_week_dates(p_odate);

    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'ECOLAB816', action_name => 'INSERTS');

    INSERT /*+ PARALLEL APPEND */ INTO xdmadm.data_816_867
            (data_who, data_key, data_data)
        SELECT DISTINCT  'ECOLAB816' || p_mfr_num ,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd data_key,
            'DT1' ||
            RPAD (NVL (LTRIM (TRANSLATE (c.cust_nm,  '*^|><@~`&','    ')),
                'Name blank in db') ,25,' ') ||
            LPAD (NVL (LTRIM (TRANSLATE (c.cust_nbr,  '*^|><@~`&','    ')),
                ' ') ,15, 0) ||
            RPAD (NVL (LTRIM (TRANSLATE (c.addr_ln_1,  '*^|><@~`&','        ')),
                'Address blank in db') ,25,' ') ||
            RPAD (NVL (LTRIM (TRANSLATE (c.city,  '*^|><@~`&','     ')),
                'City blank in db') ,18,' ') ||
            RPAD (NVL (LTRIM (TRANSLATE (c.st,  '*^|><@~`&','      ')),
                'XX') ,2,' ') ||
            LPAD (NVL (LTRIM (TRANSLATE (c.zip_cd,  '*^|><@~`&','         ')),
                ' ') ,9, 0)
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
             xdmadm.cust_corp c, xdmadm.pim_corp pim
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.cust_nbr = c.cust_nbr
        AND s.div_nbr = c.div_nbr
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.xfer_from_dt IS NULL
      UNION ALL
         SELECT DISTINCT 'ECOLAB816' || p_mfr_num,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd data_key,
            'HDR' ||
            RPAD(UPPER(p_partner_id), 15, ' ') ||
            'GEN' ||
            '816' ||
            TO_CHAR (SYSDATE, 'YYYYMMDD') ||
            RPAD(d.div_nm, 25, ' ') ||
            RPAD(d.brnch_cd, 2, ' ')
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
               xdmadm.pim_corp pim
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.xfer_from_dt IS NULL;

    COMMIT;

    SELECT COUNT(1)
    INTO g_how_many
    FROM xdmadm.data_816_867
    WHERE data_who =  'ECOLAB816' || p_mfr_num;

    DBMS_OUTPUT.put_line('***************  Number Records created: ' || g_how_many);

    DBMS_OUTPUT.put_line('END fn_extract_816_ecolab at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_timer_end := dbms_utility.get_time;
    g_run_time  := g_timer_end - g_total_timer_begin; -- milliseconds
    sp_calc_run_time(g_run_time, 'TOTAL RUN TIME : ');
    DBMS_APPLICATION_INFO.SET_MODULE(null,null);

    RETURN 0;
    EXCEPTION
        WHEN OTHERS THEN
           DBMS_APPLICATION_INFO.SET_MODULE(null,null);
            g_sqlcode := SQLCODE;
            g_message := 'fn_extract_816_ecolab sqlerror ' || g_sqlcode || ' ' || SUBSTR(SQLERRM, 1, 75);
            DBMS_OUTPUT.put_line(g_message);
            RETURN -2;
    END fn_extract_816_ecolab;


  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
FUNCTION fn_extract_867_pepsi
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
 RETURN NUMBER
   IS
  BEGIN
    DBMS_OUTPUT.put_line('BEGIN fn_extract_867_pepsi at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_total_timer_begin := dbms_utility.get_time;
    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'PEPSI867', action_name => 'DELETES');

    DELETE FROM xdmadm.data_816_867
    WHERE data_who = 'PEPSI867';

    sp_last_week_dates(p_odate);

    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'PEPSI867', action_name => 'INSERTS');

    INSERT /*+ PARALLEL APPEND */ INTO xdmadm.data_816_867
            (data_who, data_key, data_data)
        SELECT   'PEPSI867' ,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd || '2' data_key,
            'DT1' ||
            RPAD(' ', 25, ' ') ||
            RPAD (NVL (LTRIM (TRANSLATE (c.cust_nm,  '*^|><@~`&','    ')),
                'Name blank in db') ,25, ' ') ||
            LPAD (NVL (LTRIM (TRANSLATE(SUBSTR(LPAD(c.cust_nbr, 8, 0),1, 8),  '*^|><@~`&','    ')),
                ' ') ,8, 0) ||
            CASE WHEN s.trans_typ = 'CD' AND s.actl_extnd < 0 THEN
                    '76'
                 ELSE
                    '32'
            END ||
            TO_CHAR (ABS(NVL(s.qty_ship, 0)), 'S099999999999.99') ||
            RPAD(NVL(SUBSTR (pim.pim_upc_cd, 1, 14), '00000000000000'), 14, ' ') ||
            TO_CHAR (NVL (s.inv_dt, s.prcs_dt), 'YYYYMMDD') ||
            TRANSLATE (RPAD (s.inv_nbr, 15, ' ' ), '*^|><@~`&', '    ') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR( p.prod_desc ,1, 30),  '*^|><@~`&','         ')), 'Blank ProdDesc'), 30, ' ') ||
            RPAD (LTRIM (TRANSLATE(p.prod_lbl,  '*^|><@~`&','         ')), 15, ' ') ||
            RPAD (LTRIM (TRANSLATE(p.prch_pack_size,  '*^|><@~`&','         ')), 12, ' ') ||
            TO_CHAR (s.dvt_cost_extnd, 'S09999999999999.99') ||
            RPAD(p.pim_usf_std_prod_cd, 10, ' ')
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
             xdmadm.cust_corp c, xdmadm.pim_corp pim
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.cust_nbr = c.cust_nbr
        AND s.div_nbr = c.div_nbr
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.xfer_from_dt IS NULL
      UNION ALL
         SELECT  DISTINCT 'PEPSI867' ,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd || '1' data_key,
            'HDR' ||
            RPAD(UPPER(p_partner_id), 15, ' ') ||
            'GEN' ||
            '867' ||
            TO_CHAR (SYSDATE, 'YYYYMMDD') ||
            TO_CHAR (g_begin_week, 'YYYYMMDD') ||
            TO_CHAR (g_end_week, 'YYYYMMDD') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR(pim.pim_usf_mfr_nm ,1, 25),  '*^|><@~`&','         ')), 'mfrName blank IN db'), 25, ' ') ||
            RPAD (d.brnch_cd, 4, ' ') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR(d.div_nm ,1, 25),  '*^|><@~`&','         ')), ' '), 25, ' ')
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
               xdmadm.pim_corp pim
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.xfer_from_dt IS NULL
      UNION ALL
         SELECT  'PEPSI867' ,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || brnch_cd || '3' data_key,
            'SUM' ||
               TO_CHAR (SUM(ctr), 'S09999') ||
                TO_CHAR (SUM(total_volume), 'S099999999.99') ||
                 'UN'
          FROM (
              SELECT DISTINCT d.brnch_cd,  count(1) as ctr,
                    SUM(ABS(NVL(s.qty_ship, 0))) total_volume
                FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
                     xdmadm.cust_corp c, xdmadm.pim_corp pim
                WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
                AND s.cust_nbr = c.cust_nbr
                AND s.div_nbr = c.div_nbr
                AND s.prod_nbr = p.prod_nbr
                AND s.div_nbr = p.div_nbr
                AND s.div_nbr = d.div_nbr
                AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
                AND pim.pim_usf_mfr_id = p_mfr_num
                AND s.xfer_from_dt IS NULL
                GROUP BY d.brnch_cd
           )
            GROUP BY brnch_cd ;
    COMMIT;

    DBMS_OUTPUT.put_line('END fn_extract_867_pepsi at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_timer_end := dbms_utility.get_time;
    g_run_time  := g_timer_end - g_total_timer_begin; -- milliseconds
    sp_calc_run_time(g_run_time, 'TOTAL RUN TIME : ');
    DBMS_APPLICATION_INFO.SET_MODULE(null,null);

    RETURN 0;
    EXCEPTION
        WHEN OTHERS THEN
           DBMS_APPLICATION_INFO.SET_MODULE(null,null);
            g_sqlcode := SQLCODE;
            g_message := 'fn_extract_867_pepsi sqlerror ' || g_sqlcode || ' ' || SUBSTR(SQLERRM, 1, 75);
            DBMS_OUTPUT.put_line(g_message);
            RETURN -2;
    END fn_extract_867_pepsi;

 -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
FUNCTION fn_extract_867_ecolab     -- ECOLAB
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
 RETURN NUMBER
   IS
  BEGIN
    DBMS_OUTPUT.put_line('BEGIN fn_extract_867_ecolab at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_total_timer_begin := dbms_utility.get_time;
    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'ECOLAB867', action_name => 'DELETES');

    DELETE FROM xdmadm.data_816_867
    WHERE data_who = 'ECOLAB867' || p_mfr_num ;

    sp_last_week_dates(p_odate);

    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'ECOLAB867', action_name => 'INSERTS');

    INSERT /*+ PARALLEL APPEND */ INTO xdmadm.data_816_867
            (data_who, data_key, data_data)
        SELECT 'ECOLAB867' || p_mfr_num data_who,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd || '2' data_key,
            'DT1' ||
            RPAD(' ', 25, ' ') ||
            RPAD (NVL (LTRIM (TRANSLATE (c.cust_nm,  '*^|><@~`&','    ')),
                'Name blank in db') ,25, ' ') ||
            LPAD (NVL (LTRIM (TRANSLATE(SUBSTR(LPAD(c.cust_nbr, 8, 0),1, 8),  '*^|><@~`&','    ')),
                ' ') ,8, 0) ||
            CASE WHEN s.trans_typ = 'CD' AND s.actl_extnd < 0 THEN
                    '76'
                 ELSE
                    '32'
            END ||
            TO_CHAR (ABS(NVL(s.qty_ship, 0)), 'S099999999999.99') ||
            RPAD(NVL(SUBSTR (pim.pim_upc_cd, 1, 14), '00000000000000'), 14, ' ') ||
            TO_CHAR (NVL (s.inv_dt, s.prcs_dt), 'YYYYMMDD') ||
            TRANSLATE (RPAD (s.inv_nbr, 15, ' ' ), '*^|><@~`&', '    ') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR( p.prod_desc ,1, 30),  '*^|><@~`&','         ')), 'Blank ProdDesc'), 30, ' ') ||
            RPAD (LTRIM (TRANSLATE(p.prod_lbl,  '*^|><@~`&','         ')), 15, ' ') ||
            RPAD (LTRIM (TRANSLATE(p.prch_pack_size,  '*^|><@~`&','         ')), 12, ' ') ||
            TO_CHAR (s.dvt_cost_extnd, 'S09999999999999.99') ||
            RPAD(p.pim_usf_std_prod_cd, 10, ' ')
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
             xdmadm.cust_corp c, xdmadm.pim_corp pim
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.cust_nbr = c.cust_nbr
        AND s.div_nbr = c.div_nbr
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.xfer_from_dt IS NULL
      UNION ALL
         SELECT DISTINCT  'ECOLAB867' || p_mfr_num data_who,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd || '1' data_key,
            'HDR' ||
            RPAD(UPPER(p_partner_id), 15, ' ') ||
            'GEN' ||
            '867' ||
            TO_CHAR (SYSDATE, 'YYYYMMDD') ||
            TO_CHAR (g_begin_week, 'YYYYMMDD') ||
            TO_CHAR (g_end_week, 'YYYYMMDD') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR(pim.pim_usf_mfr_nm ,1, 25),  '*^|><@~`&','         ')), 'mfrName blank IN db'), 25, ' ') ||
            RPAD (d.div_nbr, 4, ' ') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR(d.div_nm ,1, 25),  '*^|><@~`&','         ')), ' '), 25, ' ')
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
               xdmadm.pim_corp pim
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.xfer_from_dt IS NULL
      UNION ALL
         SELECT 'ECOLAB867' || p_mfr_num data_who,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || brnch_cd || '3' data_key,
            'SUM' ||
               TO_CHAR (SUM(ctr), 'S09999') ||
                TO_CHAR (SUM(total_volume), 'S099999999.99') ||
                 'UN'
          FROM (
              SELECT DISTINCT d.brnch_cd,  count(1) as ctr,
                    SUM(ABS(NVL(s.qty_ship, 0))) total_volume
                FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
                     xdmadm.cust_corp c, xdmadm.pim_corp pim
                WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
                AND s.cust_nbr = c.cust_nbr
                AND s.div_nbr = c.div_nbr
                AND s.prod_nbr = p.prod_nbr
                AND s.div_nbr = p.div_nbr
                AND s.div_nbr = d.div_nbr
                AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
                AND pim.pim_usf_mfr_id = p_mfr_num
                AND s.xfer_from_dt IS NULL
                GROUP BY d.brnch_cd
           )
            GROUP BY brnch_cd ;
    COMMIT;

    DBMS_OUTPUT.put_line('END fn_extract_867_ecolab at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_timer_end := dbms_utility.get_time;
    g_run_time  := g_timer_end - g_total_timer_begin; -- milliseconds
    sp_calc_run_time(g_run_time, 'TOTAL RUN TIME : ');
    DBMS_APPLICATION_INFO.SET_MODULE(null,null);

    RETURN 0;
    EXCEPTION
        WHEN OTHERS THEN
           DBMS_APPLICATION_INFO.SET_MODULE(null,null);
            g_sqlcode := SQLCODE;
            g_message := 'fn_extract_867_ecolab sqlerror ' || g_sqlcode || ' ' || SUBSTR(SQLERRM, 1, 75);
            DBMS_OUTPUT.put_line(g_message);
            RETURN -2;
    END fn_extract_867_ecolab;

  END pkg_816_867;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_DISCO10G_USF" AS

/***********************************************************************
   * Name: PKG_DISCO10G
   * Type: Package
   * Description: Contains various user-defined functions to be used within Discoverer.
   *              This is to be owned by XDMADM.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        09/03/2012  Dave Floress     Created this package
   ************************************************************************/


FUNCTION USF_GET_PARAM ( p_param_code IN VARCHAR2 )
  RETURN NUMBER;

FUNCTION USF_GET_VENDOR_NAME (p_cmpny_vndr_nbr IN NUMBER )
  RETURN VARCHAR2;

FUNCTION USF_GET_DODAAC_CONTRACT (I_BRANCH_CODE IN VARCHAR2,I_CUSTOMER_NUMBER IN VARCHAR2)
   RETURN VARCHAR2;

END PKG_DISCO10G_USF;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_DISCO10G_USF" AS
/***********************************************************************
   * Name: PKG_DISCO10G
   * Type: Package Body
   * Description: Contains various user-defined functions to be used within Discoverer.
   *              This is to be owned by XDMADM.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        09/03/2012  Dave Floress     Created this package
   ************************************************************************/



/***********************************************************************
   * Name: USF_GET_PARAM
   * Type: Function
   * Description: Returns parameter information from XDMADM Paramater table.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0                     Frank Downey     Created this function
   ************************************************************************/
  FUNCTION USF_GET_PARAM ( p_param_code IN VARCHAR2 )
  RETURN NUMBER AS
  n_param_value_nbr xdmadm.param_value.param_value_nbr%TYPE;
  BEGIN

    SELECT PARAM_VALUE_NBR
    INTO   n_param_value_nbr
    FROM   XDMADM.PARAM_VALUE
    WHERE  PARAM_TYPE LIKE 'AR_GL_ACCT%'
           AND PARAM_CODE = p_param_code;



    RETURN n_param_value_nbr;
    EXCEPTION
        WHEN OTHERS THEN
             RETURN -1;
  END USF_GET_PARAM;



/***********************************************************************
   * Name: USF_GET_VENDOR_NAME
   * Type: Function
   * Description: Returns vendor name.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0                     Frank Downey     Created this function
   ************************************************************************/

  FUNCTION USF_GET_VENDOR_NAME (P_CMPNY_VNDR_NBR IN NUMBER )
  RETURN VARCHAR2 AS
    v_company_vendor_name VARCHAR2(30);
  BEGIN
    SELECT CMPNY_VNDR_NM
    INTO v_company_vendor_name
    FROM XDMADM.COV_CORP
    WHERE CMPNY_VNDR_NBR = p_cmpny_vndr_nbr;
    RETURN v_company_vendor_name;
  EXCEPTION
  WHEN OTHERS THEN
    RETURN 'VENDOR NOT VALID';
  END USF_GET_VENDOR_NAME;


  /***********************************************************************
   * Name: USF_GET_DODAAC_CONTRACT
   * Type: Function
   * Description: Returns V_DODAAC_CONTRACT
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0                     Krishnan Chirchabesan    Created this function
   ************************************************************************/

  FUNCTION USF_GET_DODAAC_CONTRACT (I_BRANCH_CODE IN VARCHAR2,I_CUSTOMER_NUMBER IN VARCHAR2)
   RETURN VARCHAR2 AS
   V_DODAAC_CONTRACT   VARCHAR2 (100);
BEGIN
   SELECT   D.DODACC_NUMBER || '~' || D.CONTRACT_NUMBER
     INTO   V_DODAAC_CONTRACT
     FROM   XDMADM.DODACC_CORP D
    WHERE   D.BRANCH_CODE = I_BRANCH_CODE
        AND D.CUSTOMER_NUMBER = I_CUSTOMER_NUMBER;

   RETURN V_DODAAC_CONTRACT;
EXCEPTION
   WHEN NO_DATA_FOUND
   THEN
      RETURN ' ~ ';
   WHEN OTHERS
   THEN
      RETURN 'SQL ERROR-XDMADM.USF_GET_DODAAC_CONTRACT~SQL ERROR-XDMADM.USF_GET_DODAAC_CONTRACT';
END USF_GET_DODAAC_CONTRACT;

END PKG_DISCO10G_USF;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_SLS_RGN" AS
   /***********************************************************************
   * Name: PKG_RGN_SLS_PRJ_LOAD
   * Type: Package
   * Description: Set of help functions for corporate regional sales
                   aggregate and projection tables in XDMADM.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        04/29/2011  Matt Nicol       Created this procedure
   *  1.1        07/01/2011  Matt Nicol       Update to use YYYMMDD date format
   *  1.2        12/12/2011  Wes Holbert      Added Cols grs_tgp, net_sls, net_tgp.
   ************************************************************************/
   c_aggr_syn_nm   CONSTANT VARCHAR2(30) := 'SALES_RGN_WK_AGGR';
   c_prj_syn_nm    CONSTANT VARCHAR2(30) := 'SALES_RGN_WK_PRJ';
   c_vw_syn_nm     CONSTANT VARCHAR2(30) := 'SALES_RGN_WK_AGGR_PRJ';
   c_schema_nm     CONSTANT VARCHAR2(8) := 'XDMADM';
   c_success       CONSTANT NUMBER := 0;
   c_error         CONSTANT NUMBER := 1;

   TYPE t_aggr_dim IS RECORD(div_nbr sales_rgn_wk_aggr_a.div_nbr%TYPE,
                             pim_cls_id sales_rgn_wk_aggr_a.pim_cls_id%TYPE,
                             pim_suprcls_id_crnt sales_rgn_wk_aggr_a.pim_suprcls_id_crnt%TYPE,
                             pim_suprcls_id_actl sales_rgn_wk_aggr_a.pim_suprcls_id_actl%TYPE,
                             trd_cls sales_rgn_wk_aggr_a.trd_cls%TYPE,
                             acct_typ_cd sales_rgn_wk_aggr_a.acct_typ_cd%TYPE,
                             corp_mlt_unit_nbr sales_rgn_wk_aggr_a.corp_mlt_unit_nbr%TYPE,
                             prnt_mlt_unit_cd sales_rgn_wk_aggr_a.prnt_mlt_unit_cd%TYPE,
                             pim_brnd_typ sales_rgn_wk_aggr_a.pim_brnd_typ%TYPE);

   TYPE t_aggr_msr IS RECORD(sales sales_rgn_wk_aggr_a.grs_sls_extnd%TYPE,
                             cases sales_rgn_wk_aggr_a.unfrm_qty_ship%TYPE,
                             tagp sales_rgn_wk_aggr_a.actl_gp%TYPE,
                             pa sales_rgn_wk_aggr_a.ttl_alwnc%TYPE,
                             grs_tgp sales_rgn_wk_aggr_a.grs_tgp%TYPE,
                             net_sls sales_rgn_wk_aggr_a.net_sls%TYPE,
                             net_tgp sales_rgn_wk_aggr_a.net_tgp%TYPE);

   TYPE t_aggr_growth IS RECORD(sales   NUMBER(13,4),
                                cases   NUMBER(13,4),
                                tagp    NUMBER(13,4),
                                pa      NUMBER(13,4),
                                grs_tgp NUMBER(13,4),
                                net_sls NUMBER(13,4),
                                net_tgp NUMBER(13,4));

   FUNCTION get_fisc_wk(p_clndr_dt IN DATE)
      RETURN VARCHAR2;

   FUNCTION get_measures(p_wk_from IN VARCHAR2, p_wk_to IN VARCHAR2, p_aggr_dim IN t_aggr_dim)
      RETURN t_aggr_msr;

   FUNCTION get_wow_growth(p_wk_from IN VARCHAR2, p_wk_to IN VARCHAR2, p_aggr_dim IN t_aggr_dim)
      RETURN t_aggr_growth;

   FUNCTION get_msr_growth(p_msr_cy IN NUMBER, p_msr_ly IN NUMBER)
      RETURN NUMBER;

   FUNCTION get_msr_prj(p_msr IN t_aggr_msr, p_msr_growth IN t_aggr_growth)
      RETURN t_aggr_msr;

   FUNCTION fn_get_prev_yr_wk(p_fisc_yr_wk VARCHAR, p_prev_yr NUMBER)
      RETURN VARCHAR2
      DETERMINISTIC;

   FUNCTION fn_get_tbl_nm(p_syn_nm VARCHAR2, p_tbl_actv VARCHAR2)
      RETURN VARCHAR2;

   PROCEDURE set_syn_tbl(p_tbl_nm VARCHAR2, p_tbl_actv VARCHAR2);
END pkg_sls_rgn;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_SLS_RGN" AS
   /***********************************************************************
   * Name: PKG_RGN_SLS_PRJ_LOAD
   * Type: Package Body
   * Description: Set of help functions for corporate regional sales
                  aggregate and projection tables in XDMADM.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        04/29/2011  Matt Nicol       1. Created this procedure.
   *  2.0        12/15/2012  Wes Holbert      2. Adding grs_tgp, net_sls, net_tgp.
   ************************************************************************/
   TYPE t_wow_wks IS TABLE OF xdmadm.time_corp.fisc_yr_wk%TYPE
                        INDEX BY BINARY_INTEGER;

   r_wow_wks       t_wow_wks;

   l_wow_prev_wk   VARCHAR2(6);

   /***********************************************************************
   * Name: GET_FISC_WK
   * Type: Function
   * Description: Return fiscal week for a given calendar date
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        04/29/2011  Matt Nicol       1. Created this procedure.
   ************************************************************************/
   FUNCTION get_fisc_wk(p_clndr_dt IN DATE)
      RETURN VARCHAR2 IS
      l_fisc_yr_wk   VARCHAR2(6);
   BEGIN
      SELECT fisc_yr_wk
        INTO l_fisc_yr_wk
        FROM time_corp
       WHERE TRUNC(clndr_dt) = TRUNC(p_clndr_dt);

      RETURN l_fisc_yr_wk;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in GET_FISC_WK');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END get_fisc_wk;

   /***********************************************************************
   * Name: GET_MSR_PRJ
   * Type: Function
   * Description: Determine projected measures based on given
                  measures and growth.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        04/29/2011  Matt Nicol       1. Created this procedure.
   *  1.1        01/21/2012  Wes Holbert      2. Adding measures grs_tgp,net_sls,net_tgp.
   ************************************************************************/
   FUNCTION get_msr_prj(p_msr IN t_aggr_msr, p_msr_growth IN t_aggr_growth)
      RETURN t_aggr_msr IS
      r_aggr_msr   t_aggr_msr;
   BEGIN
      r_aggr_msr.sales   := ROUND(p_msr.sales * (1 + p_msr_growth.sales), 2);
      r_aggr_msr.cases   := ROUND(p_msr.cases * (1 + p_msr_growth.cases), 2);
      r_aggr_msr.tagp    := ROUND(p_msr.tagp * (1 + p_msr_growth.tagp), 2);
      r_aggr_msr.grs_tgp := ROUND(p_msr.grs_tgp * (1 + p_msr_growth.grs_tgp), 2);        --change 1.1
      r_aggr_msr.net_sls := ROUND(p_msr.net_sls * (1 + p_msr_growth.net_sls), 2);        --change 1.1
      r_aggr_msr.net_tgp := ROUND(p_msr.net_tgp * (1 + p_msr_growth.net_tgp), 2);        --change 1.1

      RETURN r_aggr_msr;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in GET_MSR_PRJ');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END get_msr_prj;

   /***********************************************************************
   * Name: GET_MEASURES
   * Type: Function
   * Description: Calculate measures for given dimensions for given weeks range
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        04/29/2011  Matt Nicol       1. Created this procedure.
   *  1.1        01/21/2012  Wes Holbert      2. Adding grs_tgp, net_sls, net_tgp.
   ************************************************************************/
   FUNCTION get_measures(p_wk_from IN VARCHAR2, p_wk_to IN VARCHAR2, p_aggr_dim IN t_aggr_dim)
      RETURN t_aggr_msr IS
      r_aggr_msr       t_aggr_msr;
      l_inactive_tbl   VARCHAR2(30);
      l_query          VARCHAR2(1000);
   BEGIN
      l_inactive_tbl   := pkg_sls_rgn.fn_get_tbl_nm(pkg_sls_rgn.c_aggr_syn_nm, 'N');


      /* Change 1.1 New measures added */
      SELECT    'SELECT NVL (SUM (grs_sls_extnd), 0) sales, NVL (SUM (unfrm_qty_ship), 0) cases, '
             || 'NVL (SUM (actl_gp), 0) tagp, NVL (SUM (ttl_alwnc), 0) pa, '
             || 'NVL (SUM (grs_tgp), 0) grs_tgp, NVL (SUM (net_sls), 0) net_sls, '
             || 'NVL (SUM (net_tgp), 0) net_tgp '
             || ' FROM '
             || l_inactive_tbl
             || ' WHERE ly_wtd = ''N''
                AND fisc_yr_wk BETWEEN '''
             || p_wk_from
             || '''AND'''
             || p_wk_to
             || ''' AND    div_nbr = '
             || p_aggr_dim.div_nbr
             || ' AND    pim_cls_id'
             || DECODE(p_aggr_dim.pim_cls_id, NULL, ' IS NULL', ' = ' || TO_CHAR(p_aggr_dim.pim_cls_id))
             || ' AND    pim_suprcls_id_crnt'
             || DECODE(p_aggr_dim.pim_suprcls_id_crnt,
                       NULL, ' IS NULL',
                       ' = ' || TO_CHAR(p_aggr_dim.pim_suprcls_id_crnt))
             || ' AND    pim_suprcls_id_actl'
             || DECODE(p_aggr_dim.pim_suprcls_id_actl,
                       NULL, ' IS NULL',
                       ' = ' || TO_CHAR(p_aggr_dim.pim_suprcls_id_actl))
             || ' AND    trd_cls'
             || DECODE(p_aggr_dim.trd_cls, NULL, ' IS NULL', '=''' || p_aggr_dim.trd_cls || '''')
             || ' AND    acct_typ_cd'
             || DECODE(p_aggr_dim.acct_typ_cd, NULL, ' IS NULL', '=''' || p_aggr_dim.acct_typ_cd || '''')
             || ' AND    corp_mlt_unit_nbr'
             || DECODE(p_aggr_dim.corp_mlt_unit_nbr, NULL, ' IS NULL', '=''' || p_aggr_dim.corp_mlt_unit_nbr || '''')
             || ' AND    prnt_mlt_unit_cd'
             || DECODE(p_aggr_dim.prnt_mlt_unit_cd, NULL, ' IS NULL', '=''' || p_aggr_dim.prnt_mlt_unit_cd || '''')
             || ' AND    pim_brnd_typ'
             || DECODE(p_aggr_dim.pim_brnd_typ, NULL, ' IS NULL', '=''' || p_aggr_dim.pim_brnd_typ || '''')
        INTO l_query
        FROM DUAL;

      EXECUTE IMMEDIATE l_query INTO r_aggr_msr.sales,
                                     r_aggr_msr.cases,
                                     r_aggr_msr.tagp,
                                     r_aggr_msr.pa,
                                     r_aggr_msr.grs_tgp,       --Change 1.1
                                     r_aggr_msr.net_sls,       --Change 1.1
                                     r_aggr_msr.net_tgp;       --Change 1.1

      RETURN r_aggr_msr;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in GET_MEASURES');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END get_measures;

   /***********************************************************************
   * Name: GET_WOW_GROWTH
   * Type: Function
   * Description: Loop through previous weeks to determent WoW growth for
                  each week and then average the results
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        04/29/2011  Matt Nicol       1. Created this procedure.
   *  1.1        01/21/2012  Wes Holbert      2. Adding grs_tgp,net_sls,net_tgp.
   ************************************************************************/
   FUNCTION get_wow_growth(p_wk_from IN VARCHAR2, p_wk_to IN VARCHAR2, p_aggr_dim IN t_aggr_dim)
      RETURN t_aggr_growth IS
      l_prev_wk        VARCHAR2(6);
      r_total_growth   t_aggr_growth;
      r_avg_growth     t_aggr_growth;
      l_wk_cnt         NUMBER := 0;

      CURSOR c_wow_wks(x_first_wk IN VARCHAR2, x_last_wk IN VARCHAR2) IS
           SELECT fisc_yr_wk
             FROM wkly_time_corp
            WHERE fisc_yr_wk BETWEEN x_first_wk AND x_last_wk
         ORDER BY fisc_yr_wk;

      r_cur_wk_msr     t_aggr_msr;
      r_prev_wk_msr    t_aggr_msr;
   BEGIN
      IF (l_wow_prev_wk IS NULL) THEN
         SELECT fisc_yr_wkago
           INTO l_wow_prev_wk
           FROM wkly_time_corp
          WHERE fisc_yr_wk = p_wk_from;

         --Fetch weeks
         OPEN c_wow_wks(p_wk_from, p_wk_to);

         FETCH c_wow_wks
         BULK COLLECT INTO r_wow_wks;

         CLOSE c_wow_wks;
      END IF;

      r_prev_wk_msr          := get_measures(l_wow_prev_wk, l_wow_prev_wk, p_aggr_dim);
      r_total_growth.sales   := 0;
      r_total_growth.cases   := 0;
      r_total_growth.tagp    := 0;
      r_total_growth.grs_tgp := 0;       --Change 1.1
      r_total_growth.net_sls := 0;       --Change 1.1
      r_total_growth.net_tgp := 0;       --Change 1.1

      FOR i IN r_wow_wks.FIRST .. r_wow_wks.LAST LOOP
         r_cur_wk_msr           := get_measures(r_wow_wks(i), r_wow_wks(i), p_aggr_dim);

         r_total_growth.sales   := r_total_growth.sales + get_msr_growth(r_cur_wk_msr.sales, r_prev_wk_msr.sales);
         r_total_growth.cases   := r_total_growth.cases + get_msr_growth(r_cur_wk_msr.cases, r_prev_wk_msr.cases);
         r_total_growth.tagp    := r_total_growth.tagp + get_msr_growth(r_cur_wk_msr.tagp, r_prev_wk_msr.tagp);
         r_total_growth.grs_tgp := r_total_growth.grs_tgp + get_msr_growth(r_cur_wk_msr.grs_tgp, r_prev_wk_msr.grs_tgp);    --Change 1.1
         r_total_growth.net_sls := r_total_growth.net_sls + get_msr_growth(r_cur_wk_msr.net_sls, r_prev_wk_msr.net_sls);    --Change 1.1
         r_total_growth.net_tgp := r_total_growth.net_tgp + get_msr_growth(r_cur_wk_msr.net_tgp, r_prev_wk_msr.net_tgp);    --Change 1.1

         r_prev_wk_msr          := r_cur_wk_msr;
         l_wk_cnt               := l_wk_cnt + 1;
      END LOOP;

      r_avg_growth.sales     := r_total_growth.sales / l_wk_cnt;

      r_avg_growth.cases     := r_total_growth.cases / l_wk_cnt;
      r_avg_growth.tagp      := r_total_growth.tagp / l_wk_cnt;
      r_avg_growth.grs_tgp   := r_total_growth.grs_tgp / l_wk_cnt;               --Change 1.1
      r_avg_growth.net_sls   := r_total_growth.net_sls / l_wk_cnt;               --Change 1.1
      r_avg_growth.net_tgp   := r_total_growth.net_tgp / l_wk_cnt;               --Change 1.1

      RETURN r_avg_growth;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in GET_WOW_GROWTH');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END get_wow_growth;

   /***********************************************************************
   * Name: GET_MSR_GROWTH
   * Type: Function
   * Description: Calculate growth given two measures. The growth is capped at
                  200% to prevent anomolies.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        04/29/2011  Matt Nicol       1. Created this procedure.
   ************************************************************************/
   FUNCTION get_msr_growth(p_msr_cy IN NUMBER, p_msr_ly IN NUMBER)
      RETURN NUMBER IS
      l_growth   NUMBER;
   BEGIN
      IF (p_msr_ly <> 0) THEN
         l_growth   := ROUND((p_msr_cy - p_msr_ly) / p_msr_ly, 4);

         IF (l_growth > 2) THEN
            l_growth   := 2;
         END IF;

         IF (l_growth < -2) THEN
            l_growth   := -2;
         END IF;
      ELSE
         l_growth   := 0;
      END IF;

      RETURN l_growth;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in GET_MSR_GROWTH');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END get_msr_growth;

   /***********************************************************************
   * Name: FN_GET_PREV_YR_WK
   * Type: Function
   * Description: Calculate corresponding fiscal week in previous year for
                  given fiscal week and number of years.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        04/29/2011  Matt Nicol       1. Created this procedure.
   ************************************************************************/
   FUNCTION fn_get_prev_yr_wk(p_fisc_yr_wk VARCHAR, p_prev_yr NUMBER)
      RETURN VARCHAR2
      DETERMINISTIC IS
      l_prev_fisc_yr_wk   VARCHAR2(6);
      l_prev_yr           VARCHAR2(4);
      l_fisc_wk_of_yr     VARCHAR2(2);
      l_fisc_yr           NUMBER;
      l_wks_in_prev_yr    NUMBER;
   BEGIN
      l_fisc_yr         := SUBSTR(p_fisc_yr_wk, 1, 4);
      l_prev_yr         := l_fisc_yr - p_prev_yr;
      l_fisc_wk_of_yr   := SUBSTR(p_fisc_yr_wk, 5, 2);

      SELECT fisc_wk_of_yr
        INTO l_wks_in_prev_yr
        FROM xdmadm.time_corp
       WHERE clndr_dt = l_prev_yr || '1231';

      IF (l_fisc_wk_of_yr = 53) THEN
         l_prev_fisc_yr_wk   := l_prev_yr || '52';
      ELSIF (l_fisc_wk_of_yr BETWEEN 49 AND 52) THEN
         IF (l_wks_in_prev_yr = 53) THEN
            l_prev_fisc_yr_wk   := l_prev_yr || TO_CHAR(l_fisc_wk_of_yr + 1);
         ELSE
            l_prev_fisc_yr_wk   := l_prev_yr || l_fisc_wk_of_yr;
         END IF;
      ELSE
         l_prev_fisc_yr_wk   := l_prev_yr || l_fisc_wk_of_yr;
      END IF;

      RETURN l_prev_fisc_yr_wk;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in FN_GET_PREV_YR_WK');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END fn_get_prev_yr_wk;

   /***********************************************************************
   * Name: FN_GET_TBL_NM
   * Type: Function
   * Description: Get table name for given synonym and active flag
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        04/29/2011  Matt Nicol       1. Created this procedure.
   ************************************************************************/
   FUNCTION fn_get_tbl_nm(p_syn_nm VARCHAR2, p_tbl_actv VARCHAR2)
      RETURN VARCHAR2 IS
      l_full_tbl_nm   VARCHAR2(30);
   BEGIN
      SELECT tbl_nm
        INTO l_full_tbl_nm
        FROM ctladm.syn_tbl_map
       WHERE synonym_nm = p_syn_nm
         AND actv_ind = p_tbl_actv;

      RETURN l_full_tbl_nm;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in FN_GET_TBL_NM');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END fn_get_tbl_nm;

   /***********************************************************************
   * Name: SET_SYN_TBL
   * Type: Procedure
   * Description: Set active flag for given table in synonym table.
   *  REVISIONS:
   *  Ver        Date        Author           Descriptiona
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        04/29/2011  Matt Nicol       1. Created this procedure.
   ************************************************************************/
   PROCEDURE set_syn_tbl(p_tbl_nm VARCHAR2, p_tbl_actv VARCHAR2) IS
   BEGIN
      UPDATE ctladm.syn_tbl_map
         SET actv_ind = p_tbl_actv, last_updt = SYSDATE
       WHERE tbl_nm = p_tbl_nm;

      IF (sql%ROWCOUNT = 0) THEN
         DBMS_OUTPUT.put_line('No rows to update in syn tbl map');
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in SET_SYN_TBL');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
         RAISE;
   END set_syn_tbl;
END pkg_sls_rgn;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_NON_SHIP_DIV"
AS
   /******************************************************************************
      NAME:       PKG_NON_SHIP_DIV
      PURPOSE:

      REVISIONS:
      Ver        Date        Author           Description
      ---------  ----------  ---------------  ------------------------------------
      1.0        5/21/2014   Leela          1. Created this package.
   ******************************************************************************/

   FUNCTION FN_GET_NON_SHIP_DIV_LIST_REF
      RETURN SYS_REFCURSOR;

   FUNCTION FN_GET_NON_SHIP_DIV_LIST_REF (P_IN_ODATE VARCHAR2)
      RETURN SYS_REFCURSOR;

   FUNCTION FN_GET_NON_SHIP_DIV_LIST
      RETURN VARCHAR2;

   FUNCTION FN_GET_NON_SHIP_DIV_LIST (P_IN_ODATE VARCHAR2)
      RETURN VARCHAR2;

   FUNCTION FN_CHK_NON_SHIP_DIV (P_IN_DIV_NBR VARCHAR2)
      RETURN VARCHAR2;

   FUNCTION FN_CHK_NON_SHIP_DIV (P_IN_ODATE VARCHAR2, P_IN_DIV_NBR VARCHAR2)
      RETURN VARCHAR2;
END PKG_NON_SHIP_DIV;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_NON_SHIP_DIV"
AS
   /******************************************************************************
      NAME:       PKG_NON_SHIP_DIV
      PURPOSE:f

      REVISIONS:
         Ver        Date        Author           Description
         ---------  ----------  ---------------  ------------------------------------
         1.0        5/21/2014   Leela          1. Created this package.
      ******************************************************************************/
   FUNCTION FN_GET_NON_SHIP_DIV_LIST_REF
      RETURN SYS_REFCURSOR
   IS
      /******************************************************************************
         NAME: FN_GET_NON_SHIP_DIV_LIST
         TYPE: Function
         USAGE:FN_GET_NON_SHIP_DIV_LIST;

         PURPOSE:   This function is used to retrive list of non shipping divisions.

         REVISIONS:
         Ver        Date        Author           Description
         ---------  ----------  ---------------  ------------------------------------
          1.0       5/21/2014    Leela          1.Non shipping diviions

      ******************************************************************************/

      v_div_list   SYS_REFCURSOR;
   BEGIN
      OPEN v_div_list FOR
           SELECT   DISTINCT param_code
             FROM   XDMADM.PARAM_VALUE
            WHERE   app_name = 'NON_SHIP_DIV'
                    AND CASE
                          WHEN param_value_chr = 'DLY'
                          THEN
                             1
                          WHEN param_value_chr = 'WK_DAY'
                               AND TO_CHAR (SYSDATE, 'D') = param_type
                          THEN
                             1
                       END = 1
         ORDER BY   PARAM_CODE;

      RETURN v_div_list;
   END FN_GET_NON_SHIP_DIV_LIST_REF;

   FUNCTION FN_GET_NON_SHIP_DIV_LIST_REF (P_IN_ODATE VARCHAR2)
      RETURN SYS_REFCURSOR
   IS
      /******************************************************************************
         NAME: FN_GET_NON_SHIP_DIV_LIST
         TYPE: Function
         USAGE:FN_GET_NON_SHIP_DIV_LIST (P_IN_ODATE VARCHAR2);

         PURPOSE:   This function is used to retrive list of non shipping divisions.

         REVISIONS:
         Ver        Date        Author           Description
         ---------  ----------  ---------------  ------------------------------------
          1.0       5/21/2014    Leela          1.Non shipping diviions

      ******************************************************************************/

      v_div_list   SYS_REFCURSOR;
   BEGIN
      OPEN v_div_list FOR
           SELECT   DISTINCT param_code
             FROM   XDMADM.PARAM_VALUE
            WHERE   app_name = 'NON_SHIP_DIV'
                    AND CASE
                          WHEN param_value_chr = 'DLY'
                          THEN
                             1
                          WHEN param_value_chr = 'WK_DAY'
                               AND TO_CHAR (TO_DATE (P_IN_ODATE, 'YYYYMMDD'),
                                            'D') = param_type
                          THEN
                             1
                       END = 1
         ORDER BY   param_code;

      RETURN v_div_list;
   END FN_GET_NON_SHIP_DIV_LIST_REF;

   FUNCTION FN_GET_NON_SHIP_DIV_LIST
      RETURN VARCHAR2
   IS
      /******************************************************************************
         NAME: FN_GET_NON_SHIP_DIV_LIST
         TYPE: Function
         USAGE:FN_GET_NON_SHIP_DIV_LIST;

         PURPOSE:   This function is used to retrive list of non shipping divisions.

         REVISIONS:
         Ver        Date        Author           Description
         ---------  ----------  ---------------  ------------------------------------
          1.0       5/21/2014    Leela          1.Non shipping diviions

      ******************************************************************************/

      v_div_list   VARCHAR2 (1000) := NULL;
   BEGIN
      FOR C1 IN (  SELECT   DISTINCT param_code
                     FROM   XDMADM.PARAM_VALUE
                    WHERE   app_name = 'NON_SHIP_DIV'
                            AND CASE
                                  WHEN param_value_chr = 'DLY'
                                  THEN
                                     1
                                  WHEN param_value_chr = 'WK_DAY'
                                       AND TO_CHAR (SYSDATE, 'D') = param_type
                                  THEN
                                     1
                               END = 1
                 ORDER BY   PARAM_CODE)
      LOOP
         v_div_list := v_div_list ||''''|| c1.param_code||'''' || ', ';
      END LOOP;

      v_div_list := RTRIM (v_div_list, ', ');

      RETURN v_div_list;
   END FN_GET_NON_SHIP_DIV_LIST;

   FUNCTION FN_GET_NON_SHIP_DIV_LIST (P_IN_ODATE VARCHAR2)
      RETURN VARCHAR2
   IS
      /******************************************************************************
         NAME: FN_GET_NON_SHIP_DIV_LIST
         TYPE: Function
         USAGE:FN_GET_NON_SHIP_DIV_LIST (P_IN_ODATE VARCHAR2);

         PURPOSE:   This function is used to retrive list of non shipping divisions.

         REVISIONS:
         Ver        Date        Author           Description
         ---------  ----------  ---------------  ------------------------------------
          1.0       5/21/2014    Leela          1.Non shipping diviions

      ******************************************************************************/

      v_div_list   VARCHAR2 (1000) := NULL;
   BEGIN
      FOR C1 IN (  SELECT   DISTINCT param_code
                     FROM   XDMADM.PARAM_VALUE
                    WHERE   app_name = 'NON_SHIP_DIV'
                            AND CASE
                                  WHEN param_value_chr = 'DLY'
                                  THEN
                                     1
                                  WHEN param_value_chr = 'WK_DAY'
                                       AND TO_CHAR (
                                             TO_DATE (P_IN_ODATE, 'YYYYMMDD'),
                                             'D'
                                          ) = param_type
                                  THEN
                                     1
                               END = 1
                 ORDER BY   param_code)
      LOOP
         v_div_list := v_div_list ||''''|| c1.param_code||'''' || ', ';
      END LOOP;

      v_div_list := RTRIM (v_div_list, ', ');

      RETURN v_div_list;
   END FN_GET_NON_SHIP_DIV_LIST;

   FUNCTION FN_CHK_NON_SHIP_DIV (P_IN_DIV_NBR VARCHAR2)
      RETURN VARCHAR2
   IS
      /******************************************************************************
         NAME: FN_CHK_NON_SHIP_DIV
         TYPE: Function
         USAGE:FN_CHK_NON_SHIP_DIV (P_IN_DIV_NBR VARCHAR2);

         PURPOSE:   This function is used to validate if a given division is a non shipping division.

         REVISIONS:
         Ver        Date        Author           Description
         ---------  ----------  ---------------  ------------------------------------
          1.0       5/21/2014    Leela          1.Non shipping diviions

      ******************************************************************************/
      V_CNT   NUMBER := 0;
   BEGIN
        SELECT   COUNT (1)
          INTO   V_CNT
          FROM   XDMADM.param_value pri
         WHERE       app_name = 'NON_SHIP_DIV'
                 AND param_code = p_in_div_nbr
                 AND CASE
                       WHEN param_value_chr = 'DLY'
                       THEN
                          1
                       WHEN param_value_chr = 'WK_DAY'
                            AND TO_CHAR (SYSDATE, 'D') = param_type
                       THEN
                          1
                    END = 1
      ORDER BY   param_code;

      IF V_CNT > 0
      THEN
         RETURN ('Y');
      ELSE
         RETURN ('N');
      END IF;
   EXCEPTION
      WHEN OTHERS
      THEN
         DBMS_OUTPUT.PUT_LINE ('ERROR MESSAGE:' || SQLERRM);
   END FN_CHK_NON_SHIP_DIV;

   FUNCTION FN_CHK_NON_SHIP_DIV (P_IN_ODATE VARCHAR2, P_IN_DIV_NBR VARCHAR2)
      RETURN VARCHAR2
   IS
      /******************************************************************************
         NAME: FN_CHK_NON_SHIP_DIV
         TYPE: Function
         USAGE:FN_CHK_NON_SHIP_DIV (P_IN_ODATE VARCHAR2, P_IN_DIV_NBR VARCHAR2);

         PURPOSE:   This function is used to validate if a given division is a non shipping division.

         REVISIONS:
         Ver        Date        Author           Description
         ---------  ----------  ---------------  ------------------------------------
          1.0       5/21/2014    Leela          1.Non shipping diviions

      ******************************************************************************/


      V_CNT   NUMBER := 0;
   BEGIN
        SELECT   COUNT (1)
          INTO   V_CNT
          FROM   XDMADM.param_value pri
         WHERE       app_name = 'NON_SHIP_DIV'
                 AND param_code = p_in_div_nbr
                 AND CASE
                       WHEN param_value_chr = 'DLY'
                       THEN
                          1
                       WHEN param_value_chr = 'WK_DAY'
                            AND TO_CHAR (TO_DATE (P_IN_ODATE, 'YYYYMMDD'), 'D') =
                                  param_type
                       THEN
                          1
                    END = 1
      ORDER BY   param_code;

      IF V_CNT > 0
      THEN
         RETURN ('Y');
      ELSE
         RETURN ('N');
      END IF;
   EXCEPTION
      WHEN OTHERS
      THEN
         DBMS_OUTPUT.PUT_LINE ('ERROR MESSAGE:' || SQLERRM);
   END FN_CHK_NON_SHIP_DIV;
END PKG_NON_SHIP_DIV;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_CPAT_MARKUP_PRICE_BASIS" AS

  /* TODO enter package declarations (types, exceptions, methods etc) here */
 FUNCTION fn_get_calculated_markup1(
    prime_deviated_cost_type     IN VARCHAR2,
    prime_markup_indicator       IN VARCHAR2,
    each_shipped                 IN NUMBER,
    vendor_inv_cost              IN NUMBER,
    price_uom                    IN VARCHAR2,
    sales_uom                    IN VARCHAR2,
    margin                       IN NUMBER,
    prime_contract_ea_markup_amt IN NUMBER,
    UNFM_QTY_SHPD                IN NUMBER,
    NET_WT_SHIPPED               IN NUMBER,
    each_conv_fctr               IN NUMBER )
   RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE;

   FUNCTION fn_get_calculated_sell_price(
    prime_deviated_cost_type IN VARCHAR2,
    each_shpd                IN NUMBER,
    vendor_inv_cost          IN NUMBER,
    allowance                IN NUMBER,
    calculated_markup        IN NUMBER,
    DEV_COST                 IN NUMBER,
    inv_freight              IN NUMBER,
    each_conv_fctr           IN NUMBER,
    price_uom                IN VARCHAR2 )
   RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE;

END PKG_CPAT_MARKUP_PRICE_BASIS;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_CPAT_MARKUP_PRICE_BASIS"
AS

FUNCTION fn_get_calculated_markup1(
    prime_deviated_cost_type     IN VARCHAR2,
    prime_markup_indicator       IN VARCHAR2,
    each_shipped                 IN NUMBER,
    vendor_inv_cost              IN NUMBER,
    price_uom                    IN VARCHAR2,
    sales_uom                    IN VARCHAR2,
    margin                       IN NUMBER,
    prime_contract_ea_markup_amt IN NUMBER,
    UNFM_QTY_SHPD                IN NUMBER,
    NET_WT_SHIPPED               IN NUMBER,
    each_conv_fctr               IN NUMBER )
  RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE
IS
  /***********************************************************************
  * Name: fn_get_calculated_markup1
  * Type: Function
  * Description: This function is called by CPAT Markup on Price Basis.
  *              This report is in Merlin BBT subject area, in the rpd
  *              "Calculated Markup1" column calls this function.
  * Revisions:
  *  Ver        Date        Author           Description
  *  ---------  ----------  ---------------  -----------------------------
  *  1.0        09/17/2014  Jawad Majeed     Created this function
  ************************************************************************/
  calculated_markup1 NUMBER;
BEGIN
  calculated_markup1 :=
  CASE
  WHEN prime_deviated_cost_type <> 'FX' OR prime_deviated_cost_type IS NULL THEN
    CASE
    WHEN prime_markup_indicator = 'P' OR prime_markup_indicator IS NULL THEN
      CASE
      WHEN each_shipped = 0 THEN
        ((vendor_inv_cost * (margin / 100)))
      WHEN each_shipped <> 0 THEN
        CASE
        WHEN sales_uom = 'EA' AND price_uom = 'EA' THEN
          (vendor_inv_cost * (margin / 100)) + prime_contract_ea_markup_amt
        WHEN sales_uom = 'CS' AND price_uom = 'LB' THEN
          (vendor_inv_cost * (margin / 100)) + prime_contract_ea_markup_amt
        WHEN sales_uom = 'CS' AND price_uom = 'CS' THEN
          CASE WHEN each_conv_fctr = 0 THEN prime_contract_ea_markup_amt ELSE
            (vendor_inv_cost * (margin / 100)) / each_conv_fctr + prime_contract_ea_markup_amt
          END
        WHEN sales_uom = 'EA' AND price_uom = 'CS' THEN
          CASE WHEN each_conv_fctr = 0 THEN prime_contract_ea_markup_amt ELSE
            (vendor_inv_cost * (margin / 100)) / each_conv_fctr + prime_contract_ea_markup_amt
          END
        END
      END
    ELSE
      CASE
      WHEN prime_markup_indicator = '#' THEN
        CASE
        WHEN price_uom = 'LB' THEN
          margin
        ELSE
          CASE WHEN UNFM_QTY_SHPD = 0 THEN 0 ELSE
          (NET_WT_SHIPPED / UNFM_QTY_SHPD) * margin
          END
        END
      WHEN prime_markup_indicator = 'D' THEN
        margin
      END
    END
  ELSE
    0
  END;
  RETURN(NVL(calculated_markup1,0));
END;

FUNCTION fn_get_calculated_sell_price(
    prime_deviated_cost_type IN VARCHAR2,
    each_shpd                IN NUMBER,
    vendor_inv_cost          IN NUMBER,
    allowance                IN NUMBER,
    calculated_markup        IN NUMBER,
    DEV_COST                 IN NUMBER,
    inv_freight              IN NUMBER,
    each_conv_fctr           IN NUMBER,
    price_uom                IN VARCHAR2 )
  RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE
IS
  /***********************************************************************
  * Name: fn_get_calculated_sell_price
  * Type: Function
  * Description: This function is called by CPAT Markup on Price Basis.
  *              This report is in Merlin BBT subject area, in the rpd
  *              "Calculated Sell Price" column calls this function.
  * Revisions:
  *  Ver        Date        Author           Description
  *  ---------  ----------  ---------------  -----------------------------
  *  1.0        09/17/2014  Jawad Majeed     Created this function
  ************************************************************************/
  calculated_sell_price NUMBER;
BEGIN
  calculated_sell_price :=
  CASE
  WHEN each_shpd = 0 THEN
    CASE
    WHEN prime_deviated_cost_type = 'AL' THEN
      vendor_inv_cost + allowance + calculated_markup
    WHEN prime_deviated_cost_type = 'D' THEN
      DEV_COST + calculated_markup
    WHEN prime_deviated_cost_type = 'F' THEN
      DEV_COST + inv_freight + calculated_markup
    WHEN prime_deviated_cost_type = 'FX' THEN
      DEV_COST
    WHEN prime_deviated_cost_type IS NULL THEN
      vendor_inv_cost + calculated_markup
    END
  ELSE
    CASE
    WHEN prime_deviated_cost_type = 'AL' THEN
      CASE
      WHEN price_uom      IN ('EA', 'CS') THEN
        CASE WHEN each_conv_fctr = 0 THEN calculated_markup ELSE
          (((vendor_inv_cost + allowance) / each_conv_fctr) + calculated_markup)
        END
      WHEN price_uom = 'LB' THEN
        vendor_inv_cost + allowance + calculated_markup
      END
    WHEN prime_deviated_cost_type = 'D' THEN
      CASE WHEN each_conv_fctr = 0 THEN calculated_markup ELSE
        (DEV_COST / each_conv_fctr) + calculated_markup
      END
    WHEN prime_deviated_cost_type = 'F' THEN
      CASE WHEN each_conv_fctr = 0 THEN calculated_markup ELSE
       ( ( DEV_COST + inv_freight) / each_conv_fctr) + calculated_markup
      END
    WHEN prime_deviated_cost_type = 'FX' THEN
      CASE WHEN each_conv_fctr = 0 THEN 0 ELSE
        DEV_COST / each_conv_fctr
      END
    WHEN prime_deviated_cost_type IS NULL THEN
      CASE WHEN each_conv_fctr = 0 THEN calculated_markup ELSE
        (vendor_inv_cost / each_conv_fctr) + calculated_markup
      END
    END
  END ;
  RETURN(NVL(calculated_sell_price,0));
END;

END PKG_CPAT_MARKUP_PRICE_BASIS;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_FWB_CMDTY" IS
   c_success   CONSTANT NUMBER := 0;
   c_error     CONSTANT NUMBER := 1;

   FUNCTION fn_fwb_mdl_bridge_cmdty_load(p_odate IN VARCHAR2)
      RETURN NUMBER;

   FUNCTION fn_fwb_mdl_cmdty_updt(p_odate IN VARCHAR2)
      RETURN NUMBER;

   FUNCTION get_opn_po_lic(p_prod_nbr     IN VARCHAR2,
                           p_div_nbr      IN NUMBER,
                           p_arvl_dt      IN DATE)
      RETURN NUMBER;
END pkg_fwb_cmdty;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_FWB_CMDTY" IS
   FUNCTION fn_fwb_mdl_bridge_cmdty_load(p_odate IN VARCHAR2)
      RETURN NUMBER IS
      /***********************************************************************
      * Name: FN_FWB_MDL_BRIDGE_CMDTY_LOAD
      * Type: Function
      * Description: Populate FWB_MDL_BRIDGE with data from PO_CORP and OPO_CORP table
      *              by matching PO records to products with guidance in the FWB_MDL
      *              table. This only processes weekly commodity products.
      * Revisions:
      *  Ver        Date        Author             Description
      *  ---------  ----------  ---------------------------------------------
      *  1.0        10/09/2013  Matt Nicol         Created this function
      *  2.0        06/10/2015  Bob Young          Commodity Reporting
      *  3.0        02/28/2019  Prasad madicherla  Commodity Reporting

      ************************************************************************/

      l_eff_dt        DATE;
      l_odate         DATE := TO_DATE(p_odate, 'YYYYMMDD');
      l_wk_strt_day   NUMBER;

      -- Get all POs that are recieved but not in the MDL
      CURSOR c_rcvd_pos IS
         SELECT po.po_nbr,
                po.div_nbr,
                po.prod_nbr,
                po.po_rcvd_dt,
-- 2.0 Modify for catch weight  (inv.qty_on_hnd_actl * (po.last_inv_cost_per_unit - inv.last_inv_cost)) invnty_gain_loss
               ((inv.qty_on_hnd_actl * decode(PO.CTCHWGHT_IND ,'Y',PR.NET_WT,1)) * (po.last_inv_cost_per_unit - inv.last_inv_cost)) invnty_gain_loss
           FROM xdmadm.po_corp po, xdmadm.invnty_trk_corp_dly inv, xdmadm.prod_corp pr
          WHERE po.trans_typ = 'PR'
            AND po.prod_nbr = inv.prod_nbr
            AND po.div_nbr = inv.div_nbr
            AND po.div_nbr = pr.div_nbr
            AND po.prod_nbr = pr.prod_nbr
            AND inv.invnty_dt = po.po_rcvd_dt - 1
            AND EXISTS (SELECT 'X'
                          FROM xdmadm.fwb_mdl_bridge brg, xdmadm.fwb_mdl fwb
                         WHERE fwb.prod_nbr = brg.prod_nbr
                           AND fwb.div_nbr = brg.div_nbr
                           AND fwb.fwb_cyc_id = brg.fwb_cyc_id
                           AND brg.po_nbr = po.po_nbr
                           AND brg.div_nbr = po.div_nbr
                           AND TRUNC(brg.po_rcvd_dt) = TO_DATE('01-01-1900', 'DD-MM-YYYY')
                           AND fwb.fwb_cyc_typ = 'WEEKLY');

      TYPE t_rcvd_pos IS TABLE OF c_rcvd_pos%ROWTYPE;

      r_rcvd_pos      t_rcvd_pos;
   BEGIN
      DBMS_OUTPUT.put_line('Order Date: ' || l_odate);

      -- Get Wednesday of guidance week to use for effective DATE
      SELECT param_value_nbr
        INTO l_wk_strt_day
        FROM xdmadm.param_value
       WHERE app_name = 'FORWARD_BUY'
         AND param_type = 'CMTY_GUIDANCE_WK'
         AND param_code = 'STRT_DAY';

      SELECT MAX(clndr_dt)
        INTO l_eff_dt
        FROM xdmadm.time_corp
       WHERE TO_CHAR(clndr_dt, 'D') = l_wk_strt_day
         AND clndr_dt <= l_odate;

      DBMS_OUTPUT.put_line('Effective Date: ' || l_eff_dt);

      DBMS_OUTPUT.put_line('Starting Merge from OPO_CORP');

      -- Find all OPOs that have guidance for current week and merge into FWB_MDL_BRIDGE
      MERGE INTO xdmadm.fwb_mdl_bridge brg
           USING (SELECT mdl.div_nbr,
                         TO_DATE('01-01-1900', 'DD-MM-YYYY') po_rcvd_dt,
                         po.po_nbr,
                         mdl.prod_nbr,
                         po.line_nbr,
                         po.prcs_dt,
                         mdl.fwb_cyc_id,
                         mdl.eff_dt,
                         mdl.pim_usf_std_prod_cd,
                         mdl.prch_from_vndr_nbr,
                         po.po_cnfrm_ind,
                         po.arvl_dt,
                         'X' po_cnfrm_on_tm,
                         NVL(po.cnfrm_qty_ord, 0) cnfrm_qty_ord,
                         'OPO_CORP' fwb_trans_typ,
                         SYSDATE fwb_last_updt_dt,
                         TO_CHAR(SYSDATE, 'YYYYMMDDHH24MISS') ldr_tm,
                         /* Inv Gain Loss: Project inv on hand at time of scheduled arrival date and
                                           multiply by the LIC on the PO minus the LIC at time of arrival */
                         CASE
                            WHEN (  NVL(prod.crnt_qty_on_hnd, 0)
                                  + (SELECT NVL(SUM(ttl_qty_ord), 0)
                                       FROM xdmadm.opo_corp
                                      WHERE prod_nbr = po.prod_nbr
                                        AND div_nbr = po.div_nbr
                                        AND arvl_dt < po.arvl_dt)
                                  - NVL(prod.rsrv_qty, 0)
                                  - (CASE WHEN po.arvl_dt < l_odate THEN 0 ELSE (po.arvl_dt - l_odate) END
                                     * (nvl(mdl.fcst_cases,0) / 7))) < 0 THEN  /* 2.0 use forecast cases */
                               0
                            ELSE
                               (  NVL(prod.crnt_qty_on_hnd, 0)
                                + (SELECT NVL(SUM(ttl_qty_ord), 0)
                                     FROM xdmadm.opo_corp
                                    WHERE prod_nbr = po.prod_nbr
                                      AND div_nbr = po.div_nbr
                                      AND arvl_dt < po.arvl_dt)
                                - NVL(prod.rsrv_qty, 0)
                                - (CASE WHEN po.arvl_dt < l_odate THEN 0 ELSE (po.arvl_dt - l_odate) END
                                   * (nvl(mdl.fcst_cases,0) / 7))) * decode(PO.CTCHWGHT_IND ,'Y',PROD.NET_WT,1)  /* 2.0 use forecast cases  and  catch weight*/
                               * (po.last_inv_cost_per_unit - get_opn_po_lic(po.prod_nbr, po.div_nbr, po.arvl_dt))
                         END
                            invnty_gain_loss,
                         po.ord_dt,
                         po.cnfrm_dt po_cnfrm_dt
                    FROM xdmadm.fwb_mdl mdl, xdmadm.opo_corp po, xdmadm.prod_corp prod
                   WHERE mdl.prod_nbr = po.prod_nbr
                     AND mdl.div_nbr = po.div_nbr
                     AND mdl.prod_nbr = prod.prod_nbr
                     AND mdl.div_nbr = prod.div_nbr
                     AND mdl.fwb_cyc_typ = 'WEEKLY'
                     AND mdl.eff_dt >= l_eff_dt - 14
                     AND po.ord_dt BETWEEN mdl.eff_dt AND mdl.eff_dt + 6
                     AND po.ttl_qty_ord > 0) mdl
              ON (brg.po_nbr = mdl.po_nbr
              AND brg.prod_nbr = mdl.prod_nbr
              AND brg.div_nbr = mdl.div_nbr
              AND brg.eff_dt = mdl.eff_dt
              AND brg.fwb_cyc_id = mdl.fwb_cyc_id
              --AND brg.PO_RCVD_DT = mdl.PO_RCVD_DT
              --AND brg.LINE_NBR = mdl.LINE_NBR
              --AND brg.PRCS_DT = mdl.PRCS_DT
              )
      WHEN MATCHED THEN
         UPDATE SET brg.po_rcvd_dt           = mdl.po_rcvd_dt,
                    brg.prcs_dt              = mdl.prcs_dt,
                    brg.prch_from_vndr_nbr   = mdl.prch_from_vndr_nbr,
                    brg.po_cnfrm_ind         = mdl.po_cnfrm_ind,
                    brg.arvl_dt              = mdl.arvl_dt,
                    brg.po_cnfrm_on_tm       = mdl.po_cnfrm_on_tm,
                    brg.cnfrm_qty_ord        = mdl.cnfrm_qty_ord,
                    brg.fwb_trans_typ        = mdl.fwb_trans_typ,
                    brg.fwb_last_updt_dt     = mdl.fwb_last_updt_dt,
                    brg.invnty_gain_loss     = mdl.invnty_gain_loss,
                    brg.ord_dt               = mdl.ord_dt,
                    brg.po_cnfrm_dt          = mdl.po_cnfrm_dt
      WHEN NOT MATCHED THEN
         INSERT            (div_nbr, po_rcvd_dt, po_nbr, prod_nbr, line_nbr, prcs_dt,
                            fwb_cyc_id, eff_dt, pim_usf_std_prod_cd, prch_from_vndr_nbr, po_cnfrm_ind, arvl_dt,
                            po_cnfrm_on_tm, cnfrm_qty_ord, fwb_trans_typ, fwb_last_updt_dt, ldr_tm, invnty_gain_loss,
                            ord_dt, po_cnfrm_dt)
             VALUES (mdl.div_nbr,
                     mdl.po_rcvd_dt,
                     mdl.po_nbr,
                     mdl.prod_nbr,
                     mdl.line_nbr,
                     mdl.prcs_dt,
                     mdl.fwb_cyc_id,
                     mdl.eff_dt,
                     mdl.pim_usf_std_prod_cd,
                     mdl.prch_from_vndr_nbr,
                     mdl.po_cnfrm_ind,
                     mdl.arvl_dt,
                     mdl.po_cnfrm_on_tm,
                     mdl.cnfrm_qty_ord,
                     mdl.fwb_trans_typ,
                     mdl.fwb_last_updt_dt,
                     mdl.ldr_tm,
                     mdl.invnty_gain_loss,
                     mdl.ord_dt,
                     mdl.po_cnfrm_dt);

      DBMS_OUTPUT.put_line('Merged from OPO_CORP ' || sql%ROWCOUNT || ' rows');

      -- Find all POs that have guidance for current week and merge into FWB_MDL_BRIDGE
      MERGE INTO xdmadm.fwb_mdl_bridge brg
           USING (SELECT mdl.div_nbr,
                         po.po_rcvd_dt,
                         po.po_nbr,
                         mdl.prod_nbr,
                         po.line_nbr,
                         po.prcs_dt,
                         mdl.fwb_cyc_id,
                         mdl.eff_dt,
                         mdl.pim_usf_std_prod_cd,
                         mdl.prch_from_vndr_nbr,
                         po.po_cnfrm_ind,
                         po.arvl_dt,
                         'X' po_cnfrm_on_tm,
                         NVL(po.cnfrm_qty_ord, 0) cnfrm_qty_ord,
                         'PO_CORP' fwb_trans_typ,
                         SYSDATE fwb_last_updt_dt,
                         TO_CHAR(SYSDATE, 'YYYYMMDDHH24MISS') ldr_tm,
--  2.0 adjust for catch weight  (inv.qty_on_hnd_actl * (po.last_inv_cost_per_unit - inv.last_inv_cost)) invnty_gain_loss,
                        ((inv.qty_on_hnd_actl * decode(PO.CTCHWGHT_IND ,'Y',PROD.NET_WT,1))  * (po.last_inv_cost_per_unit - inv.last_inv_cost)) invnty_gain_loss,
                         po.ord_dt,
                         po.cnfrm_dt po_cnfrm_dt
                    FROM xdmadm.fwb_mdl mdl,
                         xdmadm.po_corp po,
                         xdmadm.prod_corp prod,
                         xdmadm.invnty_trk_corp_dly inv
                   WHERE mdl.prod_nbr = po.prod_nbr
                     AND mdl.div_nbr = po.div_nbr
                     AND mdl.prod_nbr = prod.prod_nbr
                     AND mdl.div_nbr = prod.div_nbr
                     AND mdl.prod_nbr = inv.prod_nbr
                     AND mdl.div_nbr = inv.div_nbr
                     AND inv.invnty_dt = po.po_rcvd_dt - 1
                     AND mdl.fwb_cyc_typ = 'WEEKLY'
                     AND po.trans_typ = 'PR'
                     AND mdl.eff_dt >= l_eff_dt - 14
                     AND po.ord_dt BETWEEN mdl.eff_dt AND mdl.eff_dt + 6
                     AND po.rcvd_qty > 0) mdl
              ON (brg.po_nbr = mdl.po_nbr
              AND brg.prod_nbr = mdl.prod_nbr
              AND brg.div_nbr = mdl.div_nbr
              AND brg.eff_dt = mdl.eff_dt
              AND brg.fwb_cyc_id = mdl.fwb_cyc_id
              /* 2/28/2019 job was failing due to mssing fields of the key */
              AND brg.po_rcvd_dt = mdl.po_rcvd_dt
              AND brg.line_nbr = mdl.line_nbr
              AND brg.prcs_dt = mdl.prcs_dt
              )
      WHEN MATCHED THEN
         UPDATE SET --brg.po_rcvd_dt           = mdl.po_rcvd_dt,
                    --brg.prcs_dt              = mdl.prcs_dt,
                    brg.prch_from_vndr_nbr   = mdl.prch_from_vndr_nbr,
                    brg.po_cnfrm_ind         = mdl.po_cnfrm_ind,
                    brg.arvl_dt              = mdl.arvl_dt,
                    brg.po_cnfrm_on_tm       = mdl.po_cnfrm_on_tm,
                    brg.cnfrm_qty_ord        = mdl.cnfrm_qty_ord,
                    brg.fwb_trans_typ        = mdl.fwb_trans_typ,
                    brg.fwb_last_updt_dt     = mdl.fwb_last_updt_dt,
                    brg.invnty_gain_loss     = mdl.invnty_gain_loss,
                    brg.ord_dt               = mdl.ord_dt,
                    brg.po_cnfrm_dt          = mdl.po_cnfrm_dt
      WHEN NOT MATCHED THEN
         INSERT            (div_nbr, po_rcvd_dt, po_nbr, prod_nbr, line_nbr, prcs_dt,
                            fwb_cyc_id, eff_dt, pim_usf_std_prod_cd, prch_from_vndr_nbr, po_cnfrm_ind, arvl_dt,
                            po_cnfrm_on_tm, cnfrm_qty_ord, fwb_trans_typ, fwb_last_updt_dt, ldr_tm, invnty_gain_loss,
                            ord_dt, po_cnfrm_dt)
             VALUES (mdl.div_nbr,
                     mdl.po_rcvd_dt,
                     mdl.po_nbr,
                     mdl.prod_nbr,
                     mdl.line_nbr,
                     mdl.prcs_dt,
                     mdl.fwb_cyc_id,
                     mdl.eff_dt,
                     mdl.pim_usf_std_prod_cd,
                     mdl.prch_from_vndr_nbr,
                     mdl.po_cnfrm_ind,
                     mdl.arvl_dt,
                     mdl.po_cnfrm_on_tm,
                     mdl.cnfrm_qty_ord,
                     mdl.fwb_trans_typ,
                     mdl.fwb_last_updt_dt,
                     mdl.ldr_tm,
                     mdl.invnty_gain_loss,
                     mdl.ord_dt,
                     mdl.po_cnfrm_dt);

      DBMS_OUTPUT.put_line('Merged from PO_CORP ' || sql%ROWCOUNT || ' rows');


      -- Update FWB_MDL_BRIDGE for POs from previous guidnace weeks that have since been recieved
      OPEN c_rcvd_pos;

      FETCH c_rcvd_pos
      BULK COLLECT INTO r_rcvd_pos;

      FORALL i IN 1 .. r_rcvd_pos.COUNT()
         UPDATE xdmadm.fwb_mdl_bridge
            SET po_rcvd_dt = r_rcvd_pos(i).po_rcvd_dt, invnty_gain_loss = r_rcvd_pos(i).invnty_gain_loss
          WHERE prod_nbr = r_rcvd_pos(i).prod_nbr
            AND div_nbr = r_rcvd_pos(i).div_nbr
            AND po_nbr = r_rcvd_pos(i).po_nbr;

      DBMS_OUTPUT.put_line('Updated ' || sql%ROWCOUNT || ' rows for inventory gain/loss');

      CLOSE c_rcvd_pos;

      -- Delete OPO lines from FWB_MDL_BRIDGE where the line no longer exists (probably canceled) or qty is zero
      DELETE FROM xdmadm.fwb_mdl_bridge brg
            WHERE brg.fwb_trans_typ = 'OPO_CORP'
              AND EXISTS (SELECT 'X'
                            FROM xdmadm.fwb_mdl mdl
                           WHERE mdl.fwb_cyc_id = brg.fwb_cyc_id
                             AND mdl.prod_nbr = brg.prod_nbr
                             AND mdl.div_nbr = brg.div_nbr
                             AND mdl.fwb_cyc_typ = 'WEEKLY')
              AND NOT EXISTS (SELECT 'X'
                                FROM xdmadm.opo_corp
                               WHERE po_nbr = brg.po_nbr
                                 AND div_nbr = brg.div_nbr
                                 AND prod_nbr = brg.prod_nbr)
              AND EXISTS (SELECT 'X'
                            FROM xdmadm.fact_load_stat
                           WHERE fact_table = 'PO_CORP'
                             AND fact_typ_cd = 'RG'
                             AND div_nbr = brg.div_nbr
                             AND latest_prcs_dt >= l_odate);

      DBMS_OUTPUT.put_line('Deleted ' || sql%ROWCOUNT || ' rows where OPO no longer exists or QTY is zero');

      -- Delete OPO lines from FWB_MDL_BRIDGE where quantity is zero
      DELETE FROM xdmadm.fwb_mdl_bridge brg
            WHERE brg.fwb_trans_typ = 'OPO_CORP'
              AND EXISTS (SELECT 'X'
                            FROM xdmadm.fwb_mdl mdl
                           WHERE mdl.fwb_cyc_id = brg.fwb_cyc_id
                             AND mdl.prod_nbr = brg.prod_nbr
                             AND mdl.div_nbr = brg.div_nbr
                             AND mdl.fwb_cyc_typ = 'WEEKLY')
              AND EXISTS (SELECT 'X'
                            FROM xdmadm.opo_corp
                           WHERE po_nbr = brg.po_nbr
                             AND div_nbr = brg.div_nbr
                             AND prod_nbr = brg.prod_nbr
                             AND ttl_qty_ord = 0);

      DBMS_OUTPUT.put_line('Deleted ' || sql%ROWCOUNT || ' rows where OPO quantity is zero');

      COMMIT;

      RETURN c_success;
   EXCEPTION
      WHEN OTHERS THEN
         ROLLBACK;
         DBMS_OUTPUT.put_line('Unexepected error');
         DBMS_OUTPUT.put_line(SQLCODE || ': ' || SQLERRM);
         RETURN c_error;
   END fn_fwb_mdl_bridge_cmdty_load;

    FUNCTION fn_fwb_mdl_cmdty_updt(p_odate IN VARCHAR2)
      RETURN NUMBER IS
      /***********************************************************************
      * Name: FN_FWB_MDL_CMDTY_UPDT
      * Type: Function
      * Description: Update FWB_MDL table with latest product and inventory data.
      *              This only processes weekly commodity data.
      * Revisions:
      *  Ver        Date        Author           Description
      *  ---------  ----------  ---------------  -----------------------------
      *  1.0        10/09/2013  Matt Nicol       Created this function
      *  2.0        06/10/2015  Bob Young        Commodity Reporting
      *  2.1        08/06/2015  Bob Young        Freeze Date Fix
      *  3.0        09/24/2018  Prasad Madicherla   Commodity Reporting updates to add OPO TTT_QTY_ORD.
      ************************************************************************/

      l_eff_dt                      DATE;
      l_odate                       DATE := TO_DATE(p_odate, 'YYYYMMDD');
      l_wk_strt_day                 NUMBER;
      l_mrkt_cost_chng_dt           DATE;
      l_freeze_date                 DATE;
      l_arvl_freeze_date            DATE;
      l_wk_freeze_day               NUMBER;
      l_fwd_mdl_opo_arvl_dt_fltr    NUMBER;
      l_invty_dt                    DATE;
      l_max_invty_dt                DATE;

      --Get updated measures for products in FWB_MDL table for current guidance week
      CURSOR c_mdl(
         p_eff_dt                     DATE,
         p_mrkt_cost_chng_dt          DATE) IS
         WITH forecast_pivot AS           /*  1.2 Create inline view for weekly forecasts based on scpo 13 week forecasts where available/ Prism 1 week if not */
                (SELECT div_nbr, prod_nbr,cases_per_week_dssnd, week
                , AVG(fcst_qty) OVER (PARTITION BY div_nbr, prod_nbr ORDER BY week ROWS BETWEEN 12 PRECEDING AND CURRENT ROW) AS fcst_qty /* calculate a running average */
                , 'S' AS fcst_own_sys
                FROM
                (SELECT
                   P.DIV_NBR, P.PROD_NBR, NVL(P.CASES_PER_WK_DSSND,0) cases_per_week_dssnd,
-- skip current week P.CASE_FCST_TTL_WK_1,
                   P.CASE_FCST_TTL_WK_2,
                   P.CASE_FCST_TTL_WK_3,
                   P.CASE_FCST_TTL_WK_4,
                   P.CASE_FCST_TTL_WK_5,
                   P.CASE_FCST_TTL_WK_6,
                   P.CASE_FCST_TTL_WK_7,
                   P.CASE_FCST_TTL_WK_8,
                   P.CASE_FCST_TTL_WK_9,
                   P.CASE_FCST_TTL_WK_10,
                   P.CASE_FCST_TTL_WK_11,
                   P.CASE_FCST_TTL_WK_12,
                   P.CASE_FCST_TTL_WK_13
                   FROM xdmadm.INVNTY_TRK_CORP_DLY P
                   WHERE P.INVNTY_DT =   l_invty_dt
                   AND P.FCST_LST_UPDT_DT IS NOT NULL)
 /* UNPIVOT creates rows from columns also creates a week column 1-12  (note we skip the current week */
                UNPIVOT (fcst_qty FOR week IN (/*CASE_FCST_TTL_WK_1 AS 1 ,*/ CASE_FCST_TTL_WK_2 AS 1, CASE_FCST_TTL_WK_3 AS 2, CASE_FCST_TTL_WK_4 AS 3,
                               CASE_FCST_TTL_WK_5 AS 4, CASE_FCST_TTL_WK_6 AS 5, CASE_FCST_TTL_WK_7 AS 6, CASE_FCST_TTL_WK_8 AS 7,
                               CASE_FCST_TTL_WK_9 AS 8, CASE_FCST_TTL_WK_10 AS 9, CASE_FCST_TTL_WK_11 AS 10, CASE_FCST_TTL_WK_12 AS 11,
                               CASE_FCST_TTL_WK_13 AS 12
                               ))
                UNION ALL
                SELECT div_nbr, prod_nbr,NVL(P1.CASES_PER_WK_DSSND,0) cases_per_wk_dssnd, wks.week, NVL(P1.CASES_PER_WK_DSSND,0) fcst_qty, 'L' AS fcst_own_sys
                FROM xdmadm.INVNTY_TRK_CORP_DLY P1
                ,(SELECT week FROM   /* create an inline table of 12 rows to  force 12 weeks for non-scpo (prism only) products */
                  ((SELECT 1 v1, 2 v2, 3 v3, 4 v4, 5 v5, 6 v6, 7 v7, 8 v8, 9 v9 , 10 v10 , 11 v11 , 12 v12, 13 v13 FROM DUAL)
                  UNPIVOT (week FOR wk IN (v1 AS 1, v2 AS 2, v3 AS 3 , v4 AS 4, v5 AS 5, v6 AS 6, v7 AS 7, v8 AS 8, v9 AS 9, v10 AS 10, v11 AS 11, v12 AS 12 /*, v13 as 13*/ ))
                  )) WKS
                   WHERE P1.INVNTY_DT = l_invty_dt
                   AND P1.FCST_LST_UPDT_DT IS  NULL)
          SELECT prod.prod_nbr,
                prod.div_nbr,
                /* Rec Qty to Buy: Build days from guidance minus current number of days on hand.
                                   If qty is less than 0 then 0 */
                CASE
                   WHEN fwb.build_invnty_days IS NULL THEN
                      NULL
                   ELSE
                      CASE
                         WHEN ((fwb.build_invnty_days
                                - CASE
                                     WHEN NVL(fcst.fcst_qty, 0) = 0 THEN  /* use forecast cases */
                                        0
                                     ELSE
                                        (NVL(prod.crnt_qty_on_hnd, 0) + NVL(opo.opo_TTL_QTY_ORD, 0) - NVL(prod.rsrv_qty, 0))
                                        / fcst.fcst_qty  /* 2.0 use forecast cases */
                                        * 7
                                  END)
                               * NVL(fcst.fcst_qty, 0) /* 2.0 use forecast cases */
                               / 7) < 0 THEN
                            0
                         ELSE
                            (fwb.build_invnty_days
                             - CASE
                                  WHEN NVL(fcst.fcst_qty, 0) = 0 THEN
                                     0
                                  ELSE
                                     (  NVL(prod.crnt_qty_on_hnd, 0)
                                      + NVL(opo.opo_TTL_QTY_ORD, 0)
                                      - NVL(prod.rsrv_qty, 0))
                                     / fcst.fcst_qty            /* 2.0 use forecast cases */
                                     * 7
                               END)
                            * NVL(fcst.fcst_qty, 0)            /* 2.0 use forecast cases */
                            / 7
                      END
                END
                   recmd_qty_to_ord,
                CASE
                   WHEN NVL(fcst.fcst_qty, 0) = 0 THEN        /* 2.0 use forecast cases */
                      0
                   ELSE
                        (NVL(prod.crnt_qty_on_hnd, 0) + NVL(opo.opo_TTL_QTY_ORD, 0) - NVL(prod.rsrv_qty, 0))
                      / fcst.fcst_qty                         /* 2.0 use forecast cases */
                      * 7
                END
                   days_invtny_on_hnd,
                NVL(prod.crnt_qty_on_hnd, 0) qty_on_hnd_actl,
                --NVL(prod.crnt_qty_on_ord, 0) qty_on_ord_actl,
                NVL(opo.opo_TTL_QTY_ORD, 0) qty_on_ord_actl,
                NVL(prod.rsrv_qty, 0) rsrv_qty,
                CASE
                   WHEN prod.futr_mrkt_cost_eff_dt <= p_mrkt_cost_chng_dt THEN prod.futr_mrkt_cost_amt
                   ELSE prod.mrkt_cost
                END
                   mrkt_cost,
                (SELECT MAX(invnty_dt)
                   FROM xdmadm.invnty_trk_corp_dly inv
                  WHERE invnty_dt <= l_odate
                    AND inv.div_nbr = prod.div_nbr
                    AND inv.prod_nbr = prod.prod_nbr
                    AND NVL(prod.mrkt_cost, 0) <> NVL(inv.mrkt_cost, 0)),
                   mrkt_cost_last_chng_dt,
                fcst.cases_per_week_dssnd ,
                fcst.fcst_qty as fcst_cases    /* 2.0 Do not want to adjust the demand here. It is brought in on the model          */
           FROM xdmadm.fwb_mdl fwb, xdmadm.prod_corp prod, forecast_pivot fcst,
           (Select div_nbr , prod_nbr ,  sum(TTL_QTY_ORD) opo_TTL_QTY_ORD
                     from xdmadm.opo_corp
                     where 1=1
                      and arvl_dt <= l_arvl_freeze_date
                      group by div_nbr , prod_nbr  )   opo
          WHERE fwb.prod_nbr = prod.prod_nbr
            AND fwb.div_nbr = prod.div_nbr
            AND fwb.prod_nbr = opo.prod_nbr(+)
            AND fwb.div_nbr = opo.div_nbr(+)
            AND fwb.fwb_cyc_typ = 'WEEKLY'
            AND fwb.eff_dt = p_eff_dt
            AND fwb.div_nbr = fcst.div_nbr
            and fwb.prod_nbr = fcst.prod_nbr
            and trim(prod.PROD_STAT_IND) in ( '0' ,'2')
            and fcst.week = LEAST(CEIL(fwb.build_invnty_days/7),12)
             ;


      TYPE t_mdl IS TABLE OF c_mdl%ROWTYPE;

      r_mdl                 t_mdl;
   BEGIN
      DBMS_OUTPUT.put_line('Order Date: ' || l_odate);

      -- Get Wednesday of guidance week to use for effective DATE
      SELECT param_value_nbr
        INTO l_wk_strt_day
        FROM xdmadm.param_value
       WHERE app_name = 'FORWARD_BUY'
         AND param_type = 'CMTY_GUIDANCE_WK'
         AND param_code = 'STRT_DAY';

      SELECT MAX(clndr_dt)
        INTO l_eff_dt
        FROM xdmadm.time_corp
       WHERE TO_CHAR(clndr_dt, 'D') = l_wk_strt_day
         AND clndr_dt <= l_odate;

      DBMS_OUTPUT.put_line('Effective Date: ' || l_eff_dt);

         -- 1.2 Get inventory freeze day of guidance week to use for freeze DATE
   SELECT param_value_nbr
     INTO l_wk_freeze_day
     FROM xdmadm.param_value
    WHERE app_name = 'FORWARD_BUY'
      AND param_type = 'CMTY_GUIDANCE_WK'
      AND param_code = 'FREEZE_DAY';

   SELECT l_eff_dt + l_wk_freeze_day
     INTO l_freeze_date
     FROM DUAL;

   SELECT MAX(invnty_dt)
     INTO l_max_invty_dt
     FROM xdmadm.invnty_trk_corp_dly ;

   IF l_freeze_date > l_max_invty_dt            /* Adjust the freeze date if it is in the future */
     THEN l_invty_dt  := l_max_invty_dt ;
     ELSE l_invty_dt  := l_freeze_date;
   END IF;

   SELECT   param_value_nbr
        INTO l_fwd_mdl_opo_arvl_dt_fltr
        FROM xdmadm.param_value
       WHERE app_name = 'FORWARD_BUY'
       and param_type = 'FWD_MDL_OPO_ARVL_DT_FLTR'
       and param_code = 'CMTY_OPEN_ORD_ARVL_DT';

    SELECT l_odate + l_fwd_mdl_opo_arvl_dt_fltr
     INTO l_arvl_freeze_date
     FROM DUAL;


   DBMS_OUTPUT.put_line('Freeze Date: ' || l_freeze_date);
   DBMS_OUTPUT.put_line('Inventory Date: ' || l_invty_dt);
   DBMS_OUTPUT.put_line('Arrival freeze Date: ' || l_arvl_freeze_date);


      -- Get Saturday after effective date to determine when next TMC change will happen
      SELECT MIN(clndr_dt)
        INTO l_mrkt_cost_chng_dt
        FROM xdmadm.time_corp
       WHERE clndr_day_of_wk = 7
         AND clndr_dt >= l_eff_dt;



      DBMS_OUTPUT.put_line('Next TMC Change Date: ' || l_mrkt_cost_chng_dt);

      -- Update rows in FWB_MDL for current guidance week
      OPEN c_mdl(l_eff_dt, l_mrkt_cost_chng_dt);

      FETCH c_mdl
      BULK COLLECT INTO r_mdl;


      FORALL i IN 1 .. r_mdl.COUNT()
         UPDATE xdmadm.fwb_mdl
            SET recmd_qty_to_ord         = r_mdl(i).recmd_qty_to_ord,
                days_invtny_on_hnd       = r_mdl(i).days_invtny_on_hnd,
                qty_on_hnd_actl          = r_mdl(i).qty_on_hnd_actl,
                qty_on_ord_actl          = r_mdl(i).qty_on_ord_actl,
                rsrv_qty                 = r_mdl(i).rsrv_qty,
                mrkt_cost                = r_mdl(i).mrkt_cost,
                mrkt_cost_last_chng_dt   = r_mdl(i).mrkt_cost_last_chng_dt,
                cases_per_wk_dssnd       = r_mdl(i).cases_per_week_dssnd,  /* 2.0 do not adjust demand*/
                fcst_cases               = r_mdl(i).fcst_cases,
                fwb_last_updt_dt         = SYSDATE
          WHERE prod_nbr = r_mdl(i).prod_nbr
            AND div_nbr = r_mdl(i).div_nbr
            AND eff_dt = l_eff_dt
            AND fwb_cyc_typ = 'WEEKLY';

      DBMS_OUTPUT.put_line('Updated ' || sql%ROWCOUNT || ' rows');

      CLOSE c_mdl;

      COMMIT;

      RETURN c_success;
   EXCEPTION
      WHEN OTHERS THEN
         ROLLBACK;
         DBMS_OUTPUT.put_line('Unexepected error');
         DBMS_OUTPUT.put_line(SQLCODE || ': ' || SQLERRM);
         RETURN c_error;
   END fn_fwb_mdl_cmdty_updt;



   FUNCTION get_opn_po_lic(p_prod_nbr     IN VARCHAR2,
                           p_div_nbr      IN NUMBER,
                           p_arvl_dt      IN DATE)
      RETURN NUMBER IS
      /***********************************************************************
      * Name: GET_OPN_PO_LIC
      * Type: Function
      * Description: Fetches the LIC for product to calculate inventory
      *              gain/loss.
      * Revisions:
      *  Ver        Date        Author           Description
      *  ---------  ----------  ---------------  -----------------------------
      *  1.0        10/09/2013  Matt Nicol       Created this function
      ************************************************************************/

      l_last_cost   NUMBER;
   BEGIN
      BEGIN
         SELECT last_inv_cost_per_unit
           INTO l_last_cost
           FROM (  SELECT last_inv_cost_per_unit
                     FROM xdmadm.opo_corp
                    WHERE prod_nbr = p_prod_nbr
                      AND div_nbr = p_div_nbr
                      AND arvl_dt < p_arvl_dt
                 ORDER BY arvl_dt DESC)
          WHERE ROWNUM = 1;
      EXCEPTION
         WHEN NO_DATA_FOUND THEN
            SELECT last_inv_cost
              INTO l_last_cost
              FROM xdmadm.prod_corp
             WHERE prod_nbr = p_prod_nbr
               AND div_nbr = p_div_nbr;
      END;

      RETURN l_last_cost;
   END get_opn_po_lic;
END pkg_fwb_cmdty;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_CPAT_MARGIN_DEV_BASIS" AS

  /* TODO enter package declarations (types, exceptions, methods etc) here */
  FUNCTION          get_dev_cost_full_cs_margin1(prime_deviated_cost_type IN VARCHAR2,
                                             prime_markup_indicator IN VARCHAR2,
                                             price_uom IN VARCHAR2,
                                             dev_cost_margin IN NUMBER,
                                             NET_WT_SHIPPED In NUMBER,
                                             UNFM_QTY_SHPD IN NUMBER,
                                             DEV_COST IN NUMBER,
                                             inv_freight IN NUMBER,
                                             prime_price_basis_amount IN NUMBER,
                                             dev_cost_allowance IN NUMBER
                                             )
   RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE;

   FUNCTION          get_dev_cost_full_cs_sellprice(prime_deviated_cost_type IN VARCHAR2,
                                             DEV_COST IN NUMBER,
                                             inv_freight IN NUMBER,
                                             dev_cost_allowance IN NUMBER,
                                             vendor_cost IN NUMBER,
                                             dev_cost_calc_full_csmargin IN NUMBER
                                             )
   RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE;

  FUNCTION          get_dev_cost_calceach_csmargin(prime_deviated_cost_type IN VARCHAR2,
                                              prime_each_markup_type IN VARCHAR2,
                                              price_uom IN VARCHAR2,
                                              sales_uom IN VARCHAR2,
                                              dev_cost_calcfull_cssell_price IN NUMBER,
                                              prime_contract_ea_markupamount IN NUMBER,
                                              each_conv_factor IN NUMBER,
                                             each_shipped IN NUMBER)
   RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE;

END PKG_CPAT_MARGIN_DEV_BASIS;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_CPAT_MARGIN_DEV_BASIS"
AS
FUNCTION get_dev_cost_full_cs_margin1(
    prime_deviated_cost_type IN VARCHAR2,
    prime_markup_indicator   IN VARCHAR2,
    price_uom                IN VARCHAR2,
    dev_cost_margin          IN NUMBER,
    NET_WT_SHIPPED           IN NUMBER,
    UNFM_QTY_SHPD            IN NUMBER,
    DEV_COST                 IN NUMBER,
    inv_freight              IN NUMBER,
    prime_price_basis_amount IN NUMBER,
    dev_cost_allowance       IN NUMBER )
  RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE
IS
  /***********************************************************************
  * Name: get_dev_cost_full_cs_margin1
  * Type: Function
  * Description: This function is called by CPAT Margin on Dev Cost Basis.
  *              This report is in Merlin BBT subject area, in the rpd
  *              DEV Cost Full CS Margin1 column calls this function.
  * Revisions:
  *  Ver        Date        Author           Description
  *  ---------  ----------  ---------------  -----------------------------
  *  1.0        09/17/2014  Jawad Majeed     Created this function
  ************************************************************************/
  dev_cost_full_cs_margin1 NUMBER;
BEGIN
  dev_cost_full_cs_margin1 :=
  CASE
  WHEN prime_deviated_cost_type = 'FX' THEN
    0
  ELSE
    CASE
    WHEN prime_markup_indicator IN ('D', 'N') OR prime_markup_indicator IS NULL THEN
      dev_cost_margin
    WHEN prime_markup_indicator = '#' THEN
      CASE
      WHEN price_uom <>'CS' THEN
        dev_cost_margin
      WHEN price_uom = 'CS' THEN
        CASE
        WHEN UNFM_QTY_SHPD = 0 THEN
          0
        ELSE
          (to_number(net_wt_shipped) / UNFM_QTY_SHPD) * dev_cost_margin
        END
      END
    WHEN prime_markup_indicator = 'P' THEN
      CASE
      WHEN (1 - dev_cost_margin ) = 0 THEN
        0
      WHEN prime_deviated_cost_type = 'AL' THEN
        ((prime_price_basis_amount + dev_cost_allowance ) / (1 - dev_cost_margin)) - (prime_price_basis_amount + dev_cost_allowance )
      WHEN prime_deviated_cost_type = 'D' THEN
        ( dev_cost / (1 - dev_cost_margin )) - dev_cost
      WHEN prime_deviated_cost_type = 'F' THEN
        ( (dev_cost + inv_freight ) / ( 1 - dev_cost_margin )) - (dev_cost + inv_freight )
      ELSE
        ( prime_price_basis_amount / (1 - dev_cost_margin)) - prime_price_basis_amount
      END
    END
  END;
  RETURN(NVL(dev_cost_full_cs_margin1,0));
END;

FUNCTION get_dev_cost_full_cs_sellprice(
    prime_deviated_cost_type    IN VARCHAR2,
    DEV_COST                    IN NUMBER,
    inv_freight                 IN NUMBER,
    dev_cost_allowance          IN NUMBER,
    vendor_cost                 IN NUMBER,
    dev_cost_calc_full_csmargin IN NUMBER )
  RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE
IS
  /***********************************************************************
  * Name: get_dev_cost_full_cs_sellprice
  * Type: Function
  * Description: This function is called by CPAT Margin on Dev Cost Basis Report.
  *              This report is in Merlin BBT subject area, in the rpd
  *              "DEV Cost Full CS Sell Price" column calls this function.
  * Revisions:
  *  Ver        Date        Author           Description
  *  ---------  ----------  ---------------  -----------------------------
  *  1.0        09/17/2014  Jawad Majeed     Created this function
  ************************************************************************/
  dev_cost_full_cs_sell_price NUMBER;
BEGIN
  dev_cost_full_cs_sell_price :=
  CASE
  WHEN prime_deviated_cost_type = 'AL' THEN
    vendor_cost + dev_cost_allowance + dev_cost_calc_full_csmargin
  WHEN prime_deviated_cost_type = 'D' THEN
    dev_cost_calc_full_csmargin + DEV_COST
  WHEN prime_deviated_cost_type = 'F' THEN
    DEV_COST + inv_freight + dev_cost_calc_full_csmargin
  WHEN prime_deviated_cost_type = 'FX' THEN
    DEV_COST
  WHEN prime_deviated_cost_type IS NULL THEN
    vendor_cost + dev_cost_calc_full_csmargin
  END;
  RETURN(NVL(dev_cost_full_cs_sell_price,0));
END;

FUNCTION get_dev_cost_calceach_csmargin(
    prime_deviated_cost_type       IN VARCHAR2,
    prime_each_markup_type         IN VARCHAR2,
    price_uom                      IN VARCHAR2,
    sales_uom                      IN VARCHAR2,
    dev_cost_calcfull_cssell_price IN NUMBER,
    prime_contract_ea_markupamount IN NUMBER,
    each_conv_factor               IN NUMBER,
    each_shipped                   IN NUMBER )
  RETURN NUMBER DETERMINISTIC PARALLEL_ENABLE
IS
  /***********************************************************************
  * Name: get_dev_cost_calceach_csmargin
  * Type: Function
  * Description: This function is called by CPAT Margin on Dev Cost Basis Report.
  *              This report is in Merlin BBT subject area, in the rpd
  *              "DEV Cost Calculated Each CS Margin" column calls this function.
  * Revisions:
  *  Ver        Date        Author           Description
  *  ---------  ----------  ---------------  -----------------------------
  *  1.0        09/17/2014  Jawad Majeed     Created this function
  ************************************************************************/
  dev_cost_calceach_csmargin NUMBER;
BEGIN
  dev_cost_calceach_csmargin :=
  CASE
  WHEN each_shipped <> 0 THEN
    CASE
    WHEN prime_deviated_cost_type <> 'FX' OR prime_deviated_cost_type IS NULL THEN
      CASE
      WHEN prime_each_markup_type = 'P' THEN
        CASE
        WHEN price_uom = 'LB' THEN
          ( prime_contract_ea_markupamount / 100) * dev_cost_calcfull_cssell_price
        WHEN sales_uom <> 'EA' AND price_uom = 'CS' THEN
          CASE WHEN each_conv_factor = 0 THEN 0 ELSE
            ( dev_cost_calcfull_cssell_price / each_conv_factor) * (prime_contract_ea_markupamount / 100)
          END
        WHEN sales_uom = 'EA' AND price_uom = 'CS' THEN
          dev_cost_calcfull_cssell_price * (prime_contract_ea_markupamount / 100)
        END
      ELSE
        CASE
        WHEN prime_each_markup_type IS NULL OR prime_each_markup_type <> 'P' THEN
          prime_contract_ea_markupamount
        END
      END
    END
  ELSE
    0
  END;
  RETURN(NVL(dev_cost_calceach_csmargin,0));
END;

END PKG_CPAT_MARGIN_DEV_BASIS;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_EDI_816_867_EXTRCT" as

FUNCTION fn_extract_816_pepsi
(
   p_partner_id   IN   VARCHAR2,
   p_odate        IN   VARCHAR2
 )
      RETURN NUMBER;

FUNCTION fn_extract_816_ecolab
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
      RETURN NUMBER;
FUNCTION fn_extract_867_pepsi
(
   p_partner_id   IN   VARCHAR2,
   p_odate        IN   VARCHAR2
 )
      RETURN NUMBER;

FUNCTION fn_extract_867_ecolab
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
      RETURN NUMBER;

END pkg_EDI_816_867_extrct;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_EDI_816_867_EXTRCT" AS
/*********************************************************************
**               Name: pkg_EDI_816_867_extrct                       **
**                                                                  **
** Created  By: Arnie Witt                          Date: 15 Oct 15 **
**                               Ver. 1.0                           **
**********************************************************************
**                              Description                         **
**                                                                  **
**      Extract functionality for pepsi and ecolab                  **
**    This package contains four callable functions:                **
**    1.  fn_extract_816_pepsi :  PEPSI                             **
**    2.  fn_extract_816_ecolab :  ECOLAB                           **
**    3.  fn_extract_867_pepsi :  PEPSI                             **
**    4.  fn_extract_867_ecolab :  ECOLAB                           **
**      all 4 functions write records to table xdmadm.data_816_867  **
**                                                                  **
**  Return:  0 - no errors occurred during processing.              **
**           <> 0 - failure during execution                        **
**                                                                  **
**                                                                  **
**********************************************************************
**  Version               Changes Description                       **
**  =======               ===================                       **
**    1.0       Initial release of this function.                   **
**   1.1 Vpatel Modified Pepsi 816 and 867 query to add             **
**                AND s.trans_typ in ('RT','CD','WC','VS')          **
**                AND d.DIV_TYP_CD <> 'CC' filters                  **
**   1.2 adding param table to pepsi extracts to get vendor list    **
**       and  exclusions                                            **
*********************************************************************/
-------------------
-- VARIABLES
-------------------
    c_limit                     CONSTANT INTEGER    := 1000;
    g_total_timer_begin         PLS_INTEGER;
    g_timer_end                 PLS_INTEGER;
    g_run_time                  PLS_INTEGER;
    g_message                   VARCHAR2(256);
    g_how_many                  INTEGER;
    g_sqlcode                   NUMBER;
    g_total_lines               INTEGER;
    g_reads                     INTEGER;
    g_this_week                 INTEGER;
    g_this_year                 INTEGER;
    g_begin_week                DATE;
    g_end_week                  DATE;
    g_records_inserted          INTEGER;
    g_cases                     NUMBER;
    g_bulk_error_count          NUMBER;
    g_idx                       NUMBER;
    g_this_idx                  NUMBER;
    g_mfr_num                   NUMBER;
    g_city                      VARCHAR2(30);
    g_last_brnch_cd             VARCHAR2(2);
    g_state                     VARCHAR2(2);
    g_zip                       VARCHAR2(15);
    g_name                      VARCHAR2(30);
    g_mfr                       VARCHAR2(30);
    v_bulk_errors                       EXCEPTION;
    PRAGMA EXCEPTION_INIT (v_bulk_errors, -24381);

CURSOR cur_816_pepsi
IS
        SELECT DISTINCT RPAD(d.div_nm, 25, ' '),
            RPAD(d.brnch_cd, 2, ' '),
            RPAD (NVL (LTRIM (TRANSLATE (c.cust_nm,  '*^|><@~`&','    ')),
                'Name blank in db') ,25,' '),
            LPAD (NVL (LTRIM (TRANSLATE (c.cust_nbr,  '*^|><@~`&','    ')),
                ' ') ,15, 0) ,
            RPAD (NVL (LTRIM (TRANSLATE (c.addr_ln_1,  '*^|><@~`&','        ')),
                'Address blank in db') ,25,' ') ,
            RPAD (NVL (LTRIM (TRANSLATE (c.city,  '*^|><@~`&','     ')),
                'CIty blank in db') ,18,' ') ,
            RPAD (NVL (LTRIM (TRANSLATE (c.st,  '*^|><@~`&','      ')),
                'XX') ,2,' '),
            LPAD (NVL (LTRIM (TRANSLATE (c.zip_cd,  '*^|><@~`&','         ')),
                ' ') ,9, 0),
            pim.pim_usf_mfr_id
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
             xdmadm.cust_corp c, xdmadm.pim_corp pim
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.cust_nbr = c.cust_nbr
        AND s.div_nbr = c.div_nbr
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND(pim.pim_usf_mfr_id in ( Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
                                     where app_name = 'PepsiEDI' and param_type = 'Vendor' )OR p.prod_nbr in  (Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
									 where app_name = 'PepsiEDI' and param_type = 'INCL_PROD_NBR' ))
        AND pim.pim_usf_mfr_id ||'-' || c.div_nbr not in  ( Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
                    where app_name = 'PepsiEDI' and param_type = 'Exclusion' )
        AND s.xfer_from_dt IS NULL
        AND c.trd_cls = '1'
        AND s.trans_typ in ('RT','CD','WC','VS')
        AND d.DIV_TYP_CD <> 'CC'
        ORDER BY RPAD(d.brnch_cd, 2, ' ');

--------------
-- collections
--------------
     TYPE ntt_number is table of NUMBER;
     TYPE ntt_date is table of DATE;
     TYPE ntt_varr IS table of VARCHAR2(100);

     nt_div_nm ntt_varr;
     nt_brnch_cd ntt_varr;
     nt_cust_nm ntt_varr;
     nt_cust_nbr ntt_varr;
     nt_addr ntt_varr;
     nt_city ntt_varr;
     nt_st ntt_varr;
     nt_zip ntt_varr;
     nt_mfr ntt_varr;


   --- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
PROCEDURE sp_calc_run_time
--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
-----------------------------------------
-- calculates the run time
-----------------------------------------
    (
        run_time_i  PLS_INTEGER,
        desc_i      VARCHAR2
    )
    IS
        v_secs NUMBER;
        v_mins NUMBER;
        v_hrs  NUMBER;
BEGIN
        v_secs    := ROUND((run_time_i) / 100, 2);
        v_hrs     := TRUNC(v_secs / 3600);
        v_secs    := v_secs - (v_hrs * 3600);
        v_mins    := TRUNC(v_secs / 60);
        v_secs    := ROUND(v_secs - (v_mins * 60));
        g_message := desc_i  || v_hrs || ' hours; ' || v_mins || ' minutes; ' || v_secs || ' seconds';
        dbms_output.put_line(g_message);

END sp_calc_run_time;
 --- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
PROCEDURE sp_last_week_dates
--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
-----------------------------------------
-- obtain calendar range for last week using ODATE
-----------------------------------------
    (
        odate_i      VARCHAR2
    )
    IS
        v_secs NUMBER;
        v_mins NUMBER;
        v_hrs  NUMBER;
BEGIN
    SELECT  fisc_yr, fisc_wk_of_yr - 1 this_week
    INTO  g_this_year , g_this_week
    FROM xdmadm.time_corp
    WHERE clndr_dt = odate_i;

    IF g_this_week = 0 THEN
        g_this_year := g_this_year - 1;
        SELECT MAX(fisc_wk_of_yr)
        INTO g_this_week
        FROM xdmadm.time_corp
        WHERE fisc_yr = g_this_year;
    END IF;

    SELECT  MIN(clndr_dt),MAX(clndr_dt)
    INTO g_begin_week, g_end_week
    FROM xdmadm.time_corp
    WHERE fisc_yr = g_this_year
    AND fisc_wk_of_yr = g_this_week;

 DBMS_OUTPUT.put_line ('DATE RANGE: ' ||  g_begin_week || ' / ' ||  g_end_week);

    EXCEPTION
        WHEN OTHERS THEN
            g_sqlcode := SQLCODE;
            g_message := 'sp_last_week_dates sqlerror ' || g_sqlcode || ' ' || SUBSTR(SQLERRM, 1, 75);
            DBMS_OUTPUT.put_line(g_message);
            RAISE;
END sp_last_week_dates;
 --- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
PROCEDURE sp_fix_data
IS
BEGIN
        /* Display a generic name if customer name has unusual characters */
        IF trim(TRANSLATE(g_name,
         '?~+=-[]_!;<>`:"{}$%* -'',^()&/.@#ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890',
         '                                 '))
                         IS NOT NULL
        THEN
                nt_cust_nm(g_this_idx) := 'Some Generic Customer    ';
        END IF;

        IF (SUBSTR(g_zip,1,5) = '00000'
                OR SUBSTR(g_zip,1,5) = '11111'
                        OR SUBSTR(g_zip,1,5) = '33333'
                        OR SUBSTR(g_zip,1,5) = '66666'
                        OR SUBSTR(g_zip,1,5) = '77777'
                        OR SUBSTR(g_zip,1,5) = '88888'
                        OR SUBSTR(g_zip,1,5) = '99999')
                OR
                   (trim(g_state) IS NULL
                OR LENGTH(trim(g_state)) < 2)
                OR
                   (g_city IS NULL
                OR LENGTH(trim(g_city)) < 3
                        OR REPLACE(UPPER(trim(g_city)), 'A', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'B', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'C', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'D', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'E', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'F', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'G', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'H', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'I', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'J', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'K', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'L', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'M', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'N', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'O', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'P', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'Q', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'R', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'S', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'T', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'U', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'V', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'W', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'X', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'Y', NULL) IS NULL
                        OR REPLACE(UPPER(trim(g_city)), 'Z', NULL) IS NULL)
                THEN
                        nt_zip(g_this_idx) := '210460000';
                        nt_st(g_this_idx) := 'MD';
                        nt_city(g_this_idx) := 'Columbia          ';
                END IF;

    EXCEPTION
        WHEN OTHERS THEN
            g_sqlcode := SQLCODE;
            g_message := 'sp_fix_data sqlerror ' || g_sqlcode || ' ' || SUBSTR(SQLERRM, 1, 75);
            DBMS_OUTPUT.put_line(g_message);
            RAISE;
END sp_fix_data;
--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
FUNCTION fn_extract_816_pepsi    -- PEPSI
(
   p_partner_id   IN   VARCHAR2,
   p_odate        IN   VARCHAR2
 )
  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
 RETURN NUMBER
   IS
  BEGIN
   DBMS_OUTPUT.put_line('BEGIN fn_extract_816_pepsi at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
   DBMS_OUTPUT.put_line('Parms: ' || p_partner_id || ' '  || p_odate);
   g_total_timer_begin := dbms_utility.get_time;
   DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'PEPSI816', action_name => 'DELETES');


   DELETE FROM xdmadm.data_816_867
   WHERE data_who = 'PEPSI816' ;

   sp_last_week_dates(p_odate);

   DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'PEPSI816', action_name => 'INSERTS');
   g_reads := 0;
   OPEN cur_816_pepsi;
   LOOP
        FETCH cur_816_pepsi BULK COLLECT INTO
             nt_div_nm, nt_brnch_cd, nt_cust_nm, nt_cust_nbr, nt_addr,
             nt_city, nt_st, nt_zip, nt_mfr LIMIT c_limit;
        IF nt_brnch_cd.COUNT > 0 THEN
                FOR vix in 1 .. nt_brnch_cd.COUNT LOOP
                   g_reads := g_reads + 1;
                   IF g_reads = 1 OR g_last_brnch_cd <> nt_brnch_cd(vix) THEN
                        g_last_brnch_cd := nt_brnch_cd(vix);
                        INSERT /*+ PARALLEL APPEND */ INTO xdmadm.data_816_867
                                (data_who, data_key, data_data)
                        VALUES ('PEPSI816' ,
                                --UPPER(RTRIM (p_partner_id)) || nt_mfr(vix) ||  nt_brnch_cd(vix),
                                UPPER(RTRIM (p_partner_id)) ||   nt_brnch_cd(vix),
                                'HDR' ||
                                RPAD(UPPER(p_partner_id), 15, ' ') ||
                                'GEN' ||
                                '816' ||
                                TO_CHAR (SYSDATE, 'YYYYMMDD') ||
                                nt_div_nm(vix) ||
                                nt_brnch_cd(vix)
                             );
                    END IF;

                    g_this_idx  := vix;
                    g_city      := nt_city(vix);
                    g_state     := nt_st(vix);
                    g_zip       := nt_zip(vix);
                    g_name      :=  nt_cust_nm(vix);
                    g_mfr       :=  nt_mfr(vix);
                    sp_fix_data;

                END LOOP;
                FORALL vix IN nt_brnch_cd.FIRST .. nt_brnch_cd.LAST  SAVE EXCEPTIONS
                    INSERT /*+ PARALLEL APPEND */ INTO xdmadm.data_816_867
                            (data_who, data_key, data_data)
                    VALUES ('PEPSI816' ,
                            --UPPER(RTRIM (p_partner_id)) || nt_mfr(vix)  || nt_brnch_cd(vix),
                            UPPER(RTRIM (p_partner_id)) ||  nt_brnch_cd(vix),
                            'DT1' ||
                            nt_cust_nm(vix) ||
                            nt_cust_nbr(vix) ||
                            nt_addr(vix) ||
                            nt_city(vix) ||
                            nt_st(vix) ||
                            nt_zip(vix)
                         );
        END IF;
        IF nt_brnch_cd.COUNT < c_limit THEN
            EXIT;
        END IF;

   END LOOP;
   CLOSE cur_816_pepsi;

    COMMIT;

    SELECT COUNT(1)
    INTO g_how_many
    FROM xdmadm.data_816_867
    WHERE data_who = 'PEPSI816';

    DBMS_OUTPUT.put_line('***************  Number Records created: ' || g_how_many);

    DBMS_OUTPUT.put_line('END fn_extract_816_pepsi at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_timer_end := dbms_utility.get_time;
    g_run_time  := g_timer_end - g_total_timer_begin; -- milliseconds
    sp_calc_run_time(g_run_time, 'TOTAL RUN TIME : ');
    DBMS_APPLICATION_INFO.SET_MODULE(null,null);

    RETURN 0;
  EXCEPTION
          WHEN v_bulk_errors THEN
                 ROLLBACK;
                 DBMS_APPLICATION_INFO.SET_MODULE(null,null);
                 g_bulk_error_count := SQL%BULK_EXCEPTIONS.count;
                 DBMS_OUTPUT.put_line('FORALL failure in : ' || 'fn_extract_816_pepsi' || '; Number of failures: ' || g_bulk_error_count);
                 g_idx := 0;
                        -- display the first 10 errors
                 FOR i IN 1 .. sql%bulk_exceptions.count LOOP
                            g_idx := g_idx + 1;
                            IF g_idx > 10 THEN
                                EXIT;
                            END IF;
                            DBMS_OUTPUT.put_line('Error: ' || i ||
                              ' Array Index: ' || SQL%BULK_EXCEPTIONS(i).error_index ||
                              ' Message: ' || SQLERRM(-SQL%BULK_EXCEPTIONS(i).ERROR_CODE));
                 END LOOP;
                  RAISE;
        WHEN OTHERS THEN
           DBMS_APPLICATION_INFO.SET_MODULE(null,null);
            g_sqlcode := SQLCODE;
            g_message := 'fn_extract_816_pepsi sqlerror ' || g_sqlcode || ' ' || SUBSTR(SQLERRM, 1, 75);
            DBMS_OUTPUT.put_line(g_message);
            RETURN -2;
END fn_extract_816_pepsi;

  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
FUNCTION fn_extract_816_ecolab
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
 RETURN NUMBER
   IS
  BEGIN
    DBMS_OUTPUT.put_line('BEGIN fn_extract_816_ecolab at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    DBMS_OUTPUT.put_line('Parms: ' || p_partner_id || ' ' || p_mfr_num
            || ' ' || p_odate);
    g_total_timer_begin := dbms_utility.get_time;
    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'ECOLAB816', action_name => 'DELETES');

    DELETE FROM xdmadm.data_816_867
    WHERE data_who = 'ECOLAB816' || p_mfr_num;

    sp_last_week_dates(p_odate);

    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'ECOLAB816', action_name => 'INSERTS');

    INSERT /*+ PARALLEL APPEND */ INTO xdmadm.data_816_867
            (data_who, data_key, data_data)
        SELECT DISTINCT  'ECOLAB816' || p_mfr_num ,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd data_key,
            'DT1' ||
            RPAD (NVL (LTRIM (TRANSLATE (c.cust_nm,  '*^|><@~`&','    ')),
                'Name blank in db') ,25,' ') ||
            LPAD (NVL (LTRIM (TRANSLATE (c.cust_nbr,  '*^|><@~`&','    ')),
                ' ') ,15, 0) ||
            RPAD (NVL (LTRIM (TRANSLATE (c.addr_ln_1,  '*^|><@~`&','        ')),
                'Address blank in db') ,25,' ') ||
            RPAD (NVL (LTRIM (TRANSLATE (c.city,  '*^|><@~`&','     ')),
                'City blank in db') ,18,' ') ||
            RPAD (NVL (LTRIM (TRANSLATE (c.st,  '*^|><@~`&','      ')),
                'XX') ,2,' ') ||
            LPAD (NVL (LTRIM (TRANSLATE (c.zip_cd,  '*^|><@~`&','         ')),
                ' ') ,9, 0)
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
             xdmadm.cust_corp c, xdmadm.pim_corp pim , xdmadm.div_corp rev_div
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.cust_nbr = c.cust_nbr
        AND s.div_nbr = c.div_nbr
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.trans_typ in ('RT','CD','WC','VS', 'CC', 'AN')
        AND s.SLS_REVENUE_DIV_NBR = rev_div.div_nbr
        AND nvl(rev_div.ACQSTN_CMPNY_NM,'USF') = 'USF'
        AND s.xfer_from_dt IS NULL
        AND c.trd_cls = '1'
      UNION ALL
         SELECT DISTINCT 'ECOLAB816' || p_mfr_num,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd data_key,
            'HDR' ||
            RPAD(UPPER(p_partner_id), 15, ' ') ||
            'GEN' ||
            '816' ||
            TO_CHAR (SYSDATE, 'YYYYMMDD') ||
            RPAD(d.div_nm, 25, ' ') ||
            RPAD(d.brnch_cd, 2, ' ')
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
               xdmadm.pim_corp pim , xdmadm.div_corp rev_div
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.trans_typ in ('RT','CD','WC','VS', 'CC', 'AN')
        AND s.SLS_REVENUE_DIV_NBR = rev_div.div_nbr
        AND nvl(rev_div.ACQSTN_CMPNY_NM,'USF') = 'USF'
        AND s.xfer_from_dt IS NULL;

    COMMIT;

    SELECT COUNT(1)
    INTO g_how_many
    FROM xdmadm.data_816_867
    WHERE data_who = 'ECOLAB816' || p_mfr_num;

    DBMS_OUTPUT.put_line('***************  Number Records created: ' || g_how_many);

    DBMS_OUTPUT.put_line('END fn_extract_816_ecolab at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_timer_end := dbms_utility.get_time;
    g_run_time  := g_timer_end - g_total_timer_begin; -- milliseconds
    sp_calc_run_time(g_run_time, 'TOTAL RUN TIME : ');
    DBMS_APPLICATION_INFO.SET_MODULE(null,null);

    RETURN 0;
    EXCEPTION
        WHEN OTHERS THEN
           DBMS_APPLICATION_INFO.SET_MODULE(null,null);
            g_sqlcode := SQLCODE;
            g_message := 'fn_extract_816_ecolab sqlerror ' || g_sqlcode || ' ' || SUBSTR(SQLERRM, 1, 75);
            DBMS_OUTPUT.put_line(g_message);
            RETURN -2;
    END fn_extract_816_ecolab;


  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
FUNCTION fn_extract_867_pepsi
(
   p_partner_id   IN   VARCHAR2,
   p_odate        IN   VARCHAR2
 )
  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
 RETURN NUMBER
   IS
  BEGIN
    DBMS_OUTPUT.put_line('BEGIN fn_extract_867_pepsi at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    DBMS_OUTPUT.put_line('Parms: ' || p_partner_id || ' ' || p_odate);
    g_total_timer_begin := dbms_utility.get_time;
    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'PEPSI867', action_name => 'DELETES');

    DELETE FROM xdmadm.data_816_867
    WHERE data_who = 'PEPSI867';

    sp_last_week_dates(p_odate);

    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'PEPSI867', action_name => 'INSERTS');

    INSERT /*+ PARALLEL APPEND */ INTO xdmadm.data_816_867
            (data_who, data_key, data_data)
        SELECT   'PEPSI867' ,
            UPPER(RTRIM (p_partner_id)) || pim.pim_usf_mfr_id || d.brnch_cd || '2' data_key,
            'DT1' ||
            RPAD(' ', 25, ' ') ||
            RPAD (NVL (LTRIM (TRANSLATE (c.cust_nm,  '*^|><@~`&','    ')),
                'Name blank in db') ,25, ' ') ||
            LPAD (NVL (LTRIM (TRANSLATE(SUBSTR(LPAD(c.cust_nbr, 8, 0),1, 8),  '*^|><@~`&','    ')),
                ' ') ,8, 0) ||
            CASE WHEN s.trans_typ = 'CD' AND sum(s.actl_extnd) < 0 THEN
                    '76'
                 ELSE
                    '32'
            END ||
            TO_CHAR (ABS(NVL(sum(s.qty_ship), 0)), 'S099999999999.99') ||
            RPAD(NVL(SUBSTR (pim.pim_upc_cd, 1, 14), '00000000000000'), 14, ' ') ||
            TO_CHAR (NVL (s.inv_dt, s.prcs_dt), 'YYYYMMDD') ||
            TRANSLATE (RPAD (s.inv_nbr, 15, ' ' ), '*^|><@~`&', '    ') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR( p.prod_desc ,1, 30),  '*^|><@~`&','         ')), 'Blank ProdDesc'), 30, ' ') ||
            RPAD (LTRIM (TRANSLATE(p.prod_lbl,  '*^|><@~`&','         ')), 15, ' ') ||
            RPAD (LTRIM (TRANSLATE(p.prch_pack_size,  '*^|><@~`&','         ')), 12, ' ') ||
            TO_CHAR (sum(s.dvt_cost_extnd), 'S09999999999999.99') ||
            RPAD(p.pim_usf_std_prod_cd, 10, ' ')
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
             xdmadm.cust_corp c, xdmadm.pim_corp pim
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.cust_nbr = c.cust_nbr
        AND s.div_nbr = c.div_nbr
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
         AND (pim.pim_usf_mfr_id in ( Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
                                     where app_name = 'PepsiEDI' and param_type = 'Vendor' ) OR p.prod_nbr in  (Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
									 where app_name = 'PepsiEDI' and param_type = 'INCL_PROD_NBR' ))
        AND pim.pim_usf_mfr_id ||'-' || c.div_nbr not in  ( Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
                    where app_name = 'PepsiEDI' and param_type = 'Exclusion' )
        AND s.xfer_from_dt IS NULL
        AND s.trans_typ in ('RT','CD','WC','VS')
        AND d.DIV_TYP_CD <> 'CC'
        AND c.trd_cls = '1'
        GROUP BY D.BRNCH_CD,
                 C.CUST_NM,
                 c.cust_nbr,
                 s.trans_typ,
                 pim.pim_upc_cd,
                 NVL (s.inv_dt, s.prcs_dt),
                 s.inv_nbr,
                 p.prod_desc,
                 p.prod_lbl,
                 p.prch_pack_size,
                 p.pim_usf_std_prod_cd,
                 pim.pim_usf_mfr_id
      UNION ALL
         SELECT  DISTINCT 'PEPSI867' ,
            UPPER(RTRIM (p_partner_id)) || pim.pim_usf_mfr_id ||  d.brnch_cd || '1' data_key,
            'HDR' ||
            RPAD(UPPER(p_partner_id), 15, ' ') ||
            'GEN' ||
            '867' ||
            TO_CHAR (SYSDATE, 'YYYYMMDD') ||
            TO_CHAR (g_begin_week, 'YYYYMMDD') ||
            TO_CHAR (g_end_week, 'YYYYMMDD') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR(pim.pim_usf_mfr_nm ,1, 25),  '*^|><@~`&','         ')), 'mfrName blank IN db'), 25, ' ') ||
            RPAD (d.brnch_cd, 4, ' ') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR(d.div_nm ,1, 25),  '*^|><@~`&','         ')), ' '), 25, ' ')
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
               xdmadm.pim_corp pim, xdmadm.cust_corp c
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.cust_nbr = c.cust_nbr
        AND s.div_nbr = c.div_nbr
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
       AND (pim.pim_usf_mfr_id in ( Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
                                     where app_name = 'PepsiEDI' and param_type = 'Vendor' ) OR p.prod_nbr in  (Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
									 where app_name = 'PepsiEDI' and param_type = 'INCL_PROD_NBR' ))
        AND pim.pim_usf_mfr_id ||'-' || c.div_nbr not in  ( Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
                    where app_name = 'PepsiEDI' and param_type = 'Exclusion' )
        AND s.xfer_from_dt IS NULL
        AND s.trans_typ in ('RT','CD','WC','VS')
        AND c.trd_cls = '1'
        AND d.DIV_TYP_CD <> 'CC'
      UNION ALL
         SELECT  'PEPSI867' ,
            UPPER(RTRIM (p_partner_id)) || pim_usf_mfr_id ||  brnch_cd || '3' data_key,
            'SUM' ||
               TO_CHAR (ctr, 'S09999') ||
                TO_CHAR (total_volume, 'S099999999.99') ||
                 'UN'
          FROM (
              SELECT d.brnch_cd,  pim.pim_usf_mfr_id , count(distinct D.BRNCH_CD||c.cust_nbr||s.trans_typ||pim.pim_upc_cd||NVL (s.inv_dt, s.prcs_dt)|| s.inv_nbr||p.prod_desc||p.pim_usf_std_prod_cd) as ctr,
                    SUM(ABS(NVL(s.qty_ship, 0))) total_volume
                FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
                     xdmadm.cust_corp c, xdmadm.pim_corp pim
                WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
                AND s.cust_nbr = c.cust_nbr
                AND s.div_nbr = c.div_nbr
                AND s.prod_nbr = p.prod_nbr
                AND s.div_nbr = p.div_nbr
                AND s.div_nbr = d.div_nbr
                AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
                AND (pim.pim_usf_mfr_id in ( Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
                                     where app_name = 'PepsiEDI' and param_type = 'Vendor' ) OR p.prod_nbr in  (Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
									 where app_name = 'PepsiEDI' and param_type = 'INCL_PROD_NBR'  ))
        AND pim.pim_usf_mfr_id ||'-' || c.div_nbr not in  ( Select  PARAM_VALUE_CHR from XDMADM.PARAM_VALUE
                    where app_name = 'PepsiEDI' and param_type = 'Exclusion' )
                AND s.xfer_from_dt IS NULL
                AND c.trd_cls = '1'
                AND s.trans_typ in ('RT','CD','WC','VS')
                AND d.DIV_TYP_CD <> 'CC'
                GROUP BY d.brnch_cd, pim.pim_usf_mfr_id
           );
    COMMIT;

    DBMS_OUTPUT.put_line('END fn_extract_867_pepsi at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_timer_end := dbms_utility.get_time;
    g_run_time  := g_timer_end - g_total_timer_begin; -- milliseconds
    sp_calc_run_time(g_run_time, 'TOTAL RUN TIME : ');
    DBMS_APPLICATION_INFO.SET_MODULE(null,null);

    RETURN 0;
    EXCEPTION
        WHEN OTHERS THEN
           DBMS_APPLICATION_INFO.SET_MODULE(null,null);
            g_sqlcode := SQLCODE;
            g_message := 'fn_extract_867_pepsi sqlerror ' || g_sqlcode || ' ' || SUBSTR(SQLERRM, 1, 75);
            DBMS_OUTPUT.put_line(g_message);
            RETURN -2;
    END fn_extract_867_pepsi;

 -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
FUNCTION fn_extract_867_ecolab     -- ECOLAB
(
   p_partner_id   IN   VARCHAR2,
   p_mfr_num      IN   NUMBER,
   p_odate        IN   VARCHAR2
 )
  -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
 RETURN NUMBER
   IS
  BEGIN
    DBMS_OUTPUT.put_line('BEGIN fn_extract_867_ecolab at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    DBMS_OUTPUT.put_line('Parms: ' || p_partner_id || ' ' || p_mfr_num
            || ' ' || p_odate);
    g_total_timer_begin := dbms_utility.get_time;
    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'ECOLAB867', action_name => 'DELETES');

    DELETE FROM xdmadm.data_816_867
    WHERE data_who = 'ECOLAB867' || p_mfr_num ;

    sp_last_week_dates(p_odate);

    DBMS_APPLICATION_INFO.SET_MODULE( module_name => 'ECOLAB867', action_name => 'INSERTS');

    INSERT /*+ PARALLEL APPEND */ INTO xdmadm.data_816_867
            (data_who, data_key, data_data)
        SELECT 'ECOLAB867' || p_mfr_num data_who,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd || '2' data_key,
            'DT1' ||
            RPAD(' ', 25, ' ') ||
            RPAD (NVL (LTRIM (TRANSLATE (c.cust_nm,  '*^|><@~`&','    ')),
                'Name blank in db') ,25, ' ') ||
            LPAD (NVL (LTRIM (TRANSLATE(SUBSTR(LPAD(c.cust_nbr, 8, 0),1, 8),  '*^|><@~`&','    ')),
                ' ') ,8, 0) ||
            CASE WHEN s.trans_typ = 'CD' AND s.actl_extnd < 0 THEN
                    '76'
                 ELSE
                    '32'
            END ||
            TO_CHAR (ABS(NVL(s.qty_ship, 0)), 'S099999999999.99') ||
            RPAD(NVL(SUBSTR (pim.pim_upc_cd, 1, 14), '00000000000000'), 14, ' ') ||
            TO_CHAR (NVL (s.inv_dt, s.prcs_dt), 'YYYYMMDD') ||
            TRANSLATE (RPAD (s.inv_nbr, 15, ' ' ), '*^|><@~`&', '    ') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR( p.prod_desc ,1, 30),  '*^|><@~`&','         ')), 'Blank ProdDesc'), 30, ' ') ||
            RPAD (LTRIM (TRANSLATE(p.prod_lbl,  '*^|><@~`&','         ')), 15, ' ') ||
            RPAD (LTRIM (TRANSLATE(p.prch_pack_size,  '*^|><@~`&','         ')), 12, ' ') ||
            TO_CHAR (s.dvt_cost_extnd, 'S09999999999999.99') ||
            RPAD(p.pim_usf_std_prod_cd, 10, ' ')
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
             xdmadm.cust_corp c, xdmadm.pim_corp pim , xdmadm.div_corp rev_div
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.cust_nbr = c.cust_nbr
        AND s.div_nbr = c.div_nbr
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.xfer_from_dt IS NULL
        AND s.trans_typ in ('RT','CD','WC','VS', 'CC', 'AN')
        AND s.SLS_REVENUE_DIV_NBR = rev_div.div_nbr
        AND nvl(rev_div.ACQSTN_CMPNY_NM,'USF') = 'USF'
        AND c.trd_cls = '1'
      UNION ALL
         SELECT DISTINCT  'ECOLAB867' || p_mfr_num data_who,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || d.brnch_cd || '1' data_key,
            'HDR' ||
            RPAD(UPPER(p_partner_id), 15, ' ') ||
            'GEN' ||
            '867' ||
            TO_CHAR (SYSDATE, 'YYYYMMDD') ||
            TO_CHAR (g_begin_week, 'YYYYMMDD') ||
            TO_CHAR (g_end_week, 'YYYYMMDD') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR(pim.pim_usf_mfr_nm ,1, 25),  '*^|><@~`&','         ')), 'mfrName blank IN db'), 25, ' ') ||
            RPAD (d.div_nbr, 4, ' ') ||
            RPAD (NVL (LTRIM (TRANSLATE(SUBSTR(d.div_nm ,1, 25),  '*^|><@~`&','         ')), ' '), 25, ' ')
        FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
               xdmadm.pim_corp pim, xdmadm.cust_corp c , xdmadm.div_corp rev_div
        WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
        AND s.cust_nbr = c.cust_nbr
        AND s.div_nbr = c.div_nbr
        AND s.prod_nbr = p.prod_nbr
        AND s.div_nbr = p.div_nbr
        AND s.div_nbr = d.div_nbr
        AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
        AND pim.pim_usf_mfr_id = p_mfr_num
        AND s.trans_typ in ('RT','CD','WC','VS', 'CC', 'AN')
        AND s.SLS_REVENUE_DIV_NBR = rev_div.div_nbr
        AND nvl(rev_div.ACQSTN_CMPNY_NM,'USF') = 'USF'
        AND s.xfer_from_dt IS NULL
      UNION ALL
         SELECT 'ECOLAB867' || p_mfr_num data_who,
            UPPER(RTRIM (p_partner_id)) || p_mfr_num || brnch_cd || '3' data_key,
            'SUM' ||
               TO_CHAR (SUM(ctr), 'S09999') ||
                TO_CHAR (SUM(total_volume), 'S099999999.99') ||
                 'UN'
          FROM (
              SELECT DISTINCT d.brnch_cd,  count(1) as ctr,
                    SUM(ABS(NVL(s.qty_ship, 0))) total_volume
                FROM xdmadm.div_corp d, xdmadm.sales_corp s, xdmadm.prod_corp p,
                     xdmadm.cust_corp c, xdmadm.pim_corp pim , xdmadm.div_corp rev_div
                WHERE s.prcs_dt BETWEEN g_begin_week AND g_end_week
                AND s.cust_nbr = c.cust_nbr
                AND s.div_nbr = c.div_nbr
                AND s.prod_nbr = p.prod_nbr
                AND s.div_nbr = p.div_nbr
                AND s.div_nbr = d.div_nbr
                AND p.pim_usf_std_prod_cd = pim.pim_usf_std_prod_cd
                AND pim.pim_usf_mfr_id = p_mfr_num
                AND s.xfer_from_dt IS NULL
                AND c.trd_cls = '1'
                AND s.trans_typ in ('RT','CD','WC','VS', 'CC', 'AN')
                AND s.SLS_REVENUE_DIV_NBR = rev_div.div_nbr
                AND nvl(rev_div.ACQSTN_CMPNY_NM,'USF') = 'USF'
                GROUP BY d.brnch_cd
           )
            GROUP BY brnch_cd ;
    COMMIT;

    DBMS_OUTPUT.put_line('END fn_extract_867_ecolab at ' || to_char(sysdate, 'yyyymmdd hh24:mi:ss'));
    g_timer_end := dbms_utility.get_time;
    g_run_time  := g_timer_end - g_total_timer_begin; -- milliseconds
    sp_calc_run_time(g_run_time, 'TOTAL RUN TIME : ');
    DBMS_APPLICATION_INFO.SET_MODULE(null,null);

    RETURN 0;
    EXCEPTION
        WHEN OTHERS THEN
           DBMS_APPLICATION_INFO.SET_MODULE(null,null);
            g_sqlcode := SQLCODE;
            g_message := 'fn_extract_867_ecolab sqlerror ' || g_sqlcode || ' ' || SUBSTR(SQLERRM, 1, 75);
            DBMS_OUTPUT.put_line(g_message);
            RETURN -2;
    END fn_extract_867_ecolab;

  END pkg_EDI_816_867_extrct ;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_SLS_RNK" AS
   FUNCTION fn_upd_sls_rnk(p_odate IN VARCHAR2)
      RETURN NUMBER;
END pkg_sls_rnk;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_SLS_RNK" AS
   /***********************************************************************
   * Name: PKG_SLS_RNK
   * Type: Package
   * Description: All functions and procedures used to build sales ranking tables:
   *              XDMADM.SLS_RNK_MTH & XDMADM.SLS_RNK_ORG
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        06/05/2013  Matt Nicol       Initial Creation
   ************************************************************************/

   g_fisc_yr       NUMBER;
   g_fisc_yr_mth   VARCHAR2(6);
   g_fisc_yr_wk    VARCHAR2(6);
   g_default_id    CONSTANT VARCHAR2(30) := 'UNKNOWN';
   g_default_nm    CONSTANT VARCHAR2(30) := 'UNKNOWN';
   g_grp_typ_cd    CONSTANT VARCHAR2(30) := 'SALES';

   PROCEDURE ins_sls_rnk_org;

   PROCEDURE ins_sls_rnk_tmp;

   PROCEDURE ins_sls_rnk_mth;

   PROCEDURE upd_sls_rnk_mth_cnt;

   PROCEDURE upd_sls_rnk_mth_rnk;

   PROCEDURE upd_sls_rnk_org_nm;

   /***********************************************************************
   * Name: FN_UPD_SLS_RNK
   * Type: Function
   * Description: Main function for building sales ranking tables
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        06/05/2013  Matt Nicol       Initial Creation
   ************************************************************************/
   FUNCTION fn_upd_sls_rnk(p_odate IN VARCHAR2)
      RETURN NUMBER IS
      c_success           CONSTANT NUMBER := 0;
      c_error             CONSTANT NUMBER := 1;

      l_return_code       NUMBER := 0;
      l_out_msg           VARCHAR2(256);
      l_odate             DATE := TO_DATE(p_odate, 'YYYYMMDD');
      l_last_sat          DATE;
      l_lastest_pa_load   DATE;
   BEGIN
      -- Get previous Saturday date and use as max processing date
      IF TRIM(TO_CHAR(l_odate, 'DAY')) = 'SATURDAY' THEN
         l_last_sat   := l_odate;
      ELSE
         l_last_sat   := TRUNC(l_odate, 'DAY') - 1;
      END IF;

      DBMS_OUTPUT.put_line('Most Recent Saturday: ' || l_last_sat);

      -- Get fiscal year, month, and week corresponding to previous Saturday
      SELECT fisc_yr, fisc_yr_mth, fisc_yr_wk
        INTO g_fisc_yr, g_fisc_yr_mth, g_fisc_yr_wk
        FROM xdmadm.time_corp
       WHERE clndr_dt = l_last_sat;

      DBMS_OUTPUT.put_line('Fisc Year: ' || g_fisc_yr);
      DBMS_OUTPUT.put_line('Fisc Month: ' || g_fisc_yr_mth);
      DBMS_OUTPUT.put_line('Fisc Week: ' || g_fisc_yr_wk);

      --Check if PAs are loaded for current week. If not, fail job.
      SELECT MAX(latest_prcs_dt)
        INTO l_lastest_pa_load
        FROM xdmadm.fact_load_stat
       WHERE fact_table = 'SALES_CORP'
         AND fact_typ_cd = 'AP';

      IF l_lastest_pa_load <= l_last_sat - 7 THEN
         DBMS_OUTPUT.put_line('PAs not loaded for week');
         RETURN 1;
      END IF;

      -- Call procedure to populate SLS_RNK_ORG_TMP table
      ins_sls_rnk_org;

      -- Call procedure to update SLS_RNK_ORG and SLS_RNK_ORG_TMP tables with names
      upd_sls_rnk_org_nm;

      -- Call procedure to populate SLS_RNK_TMP table
      ins_sls_rnk_tmp;

      -- Call procedure to populate SLS_RNK_MTH_TMP table
      ins_sls_rnk_mth;

      -- Call procedure to populate SLS_RNK_MTH_TMP table with population counts
      upd_sls_rnk_mth_cnt;

      -- Call procedure to populate SLS_RNK_MTH_TMP table with rank values
      upd_sls_rnk_mth_rnk;

      -- Truncate partition on SLS_RNK_ORG table
      l_return_code   :=
         usfdba.table_pkg.truncate_partition(l_out_msg,
                                             'XDMADM',
                                             'SLS_RNK_ORG',
                                             'SLS_RNK_ORG_' || g_fisc_yr_mth);

      DBMS_OUTPUT.put_line('Truncate table partition SLS_RNK_ORG_' || g_fisc_yr_mth || ' return code: ' || l_return_code);

      IF l_return_code <> 0 THEN
         DBMS_OUTPUT.put_line('Error: Failed to truncate table partition SLS_RNK_ORG_' || g_fisc_yr_mth);
         RETURN 1;
      END IF;

      -- Truncate partition on SLS_RNK_MTH table
      l_return_code   :=
         usfdba.table_pkg.truncate_partition(l_out_msg,
                                             'XDMADM',
                                             'SLS_RNK_MTH',
                                             'SLS_RNK_MTH_' || g_fisc_yr_mth);

      DBMS_OUTPUT.put_line('Truncate table partition SLS_RNK_MTH_' || g_fisc_yr_mth || ' return code: ' || l_return_code);

      IF l_return_code <> 0 THEN
         DBMS_OUTPUT.put_line('Error: Failed to truncate table partition SLS_RNK_MTH_' || g_fisc_yr_mth);
         RETURN 1;
      END IF;

      -- Copy data from SLS_RNK_ORG_TMP to SLS_RNK_ORG
      INSERT /*+ append */
            INTO sls_rnk_org
         (SELECT * FROM sls_rnk_org_tmp);

      DBMS_OUTPUT.put_line('Inserted ' || sql%ROWCOUNT || ' rows into SLS_RNK_ORG');

      -- Copy data from SLS_RNK_MTH_TMP to SLS_RNK_MTH
      INSERT /*+ append */
            INTO sls_rnk_mth
         (SELECT * FROM sls_rnk_mth_tmp);

      DBMS_OUTPUT.put_line('Inserted ' || sql%ROWCOUNT || ' rows into SLS_RNK_MTH');

      COMMIT;

      -- Truncate table SLS_RNK_ORG_TMP
      l_return_code   := usfdba.table_pkg.truncate_table(l_out_msg, 'XDMADM', 'SLS_RNK_ORG_TMP');
      DBMS_OUTPUT.put_line('Truncate table SLS_RNK_ORG_TMP return code: ' || l_return_code);

      IF l_return_code <> 0 THEN
         DBMS_OUTPUT.put_line('Error: Failed to truncate table SLS_RNK_ORG_TMP');
         RETURN 1;
      END IF;

      -- Truncate table SLS_RNK_MTH_TMP
      l_return_code   := usfdba.table_pkg.truncate_table(l_out_msg, 'XDMADM', 'SLS_RNK_MTH_TMP');
      DBMS_OUTPUT.put_line('Truncate table SLS_RNK_MTH_TMP return code: ' || l_return_code);

      IF l_return_code <> 0 THEN
         DBMS_OUTPUT.put_line('Error: Failed to truncate table SLS_RNK_MTH_TMP');
         RETURN 1;
      END IF;

      -- Get stats on SLS_RNK_ORG table
      l_return_code   := xdmadm.fn_getstat_tbl('XDMADM', 'SLS_RNK_ORG');
      DBMS_OUTPUT.put_line('Analyze SLS_RNK_ORG Return Code: ' || l_return_code);

      IF l_return_code <> 0 THEN
         DBMS_OUTPUT.put_line('Error: Failed to analyze table SLS_RNK_ORG');
         RETURN 1;
      END IF;

      -- Get stats on SLS_RNK_MTH table
      l_return_code   := xdmadm.fn_getstat_tbl('XDMADM', 'SLS_RNK_MTH');
      DBMS_OUTPUT.put_line('Analyze SLS_RNK_MTH Return Code: ' || l_return_code);

      IF l_return_code <> 0 THEN
         DBMS_OUTPUT.put_line('Error: Failed to analyze table SLS_RNK_MTH');
         RETURN 1;
      END IF;

      -- Update FACT_LOAD_STAT with latest processed date
      UPDATE xdmadm.fact_load_stat
         SET latest_prcs_dt = l_last_sat, prcs_dt = l_odate
       WHERE fact_table = 'SLS_RNK_MTH';

      COMMIT;
      DBMS_OUTPUT.put_line('Updated FACT_LOAD_STAT');

      RETURN 0;
   EXCEPTION
      WHEN OTHERS THEN
         ROLLBACK;
         DBMS_OUTPUT.put_line('Error processing sales ranking tables');
         DBMS_OUTPUT.put_line(SQLERRM);
         RETURN 1;
   END fn_upd_sls_rnk;

   /***********************************************************************
   * Name: INS_SLS_RNK_ORG
   * Type: Procedure
   * Description: Populates SLS_RNK_ORG table
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        06/05/2013  Matt Nicol       Initial Creation
   ************************************************************************/
   PROCEDURE ins_sls_rnk_org IS
      l_return_code             NUMBER := 0;
      l_out_msg                 VARCHAR2(256);

      l_rsm_dsm_acct_thrshld    NUMBER;
      l_tm_trtry_acct_thrshld   NUMBER;
   BEGIN
      -- Get threshhold values from param table for eligibility rules
      SELECT param_value_nbr
        INTO l_rsm_dsm_acct_thrshld
        FROM xdmadm.param_value
       WHERE app_name = 'SLS_RNK'
         AND param_type = 'NET_ACCT_THRSHLD'
         AND param_code = 'RSM_DSM';

      DBMS_OUTPUT.put_line('RSM DSM - Net account threshold value: ' || l_rsm_dsm_acct_thrshld);

      SELECT param_value_nbr
        INTO l_tm_trtry_acct_thrshld
        FROM xdmadm.param_value
       WHERE app_name = 'SLS_RNK'
         AND param_type = 'NET_ACCT_THRSHLD'
         AND param_code = 'TM_TRTRY';

      DBMS_OUTPUT.put_line('TM TRTRY - Net account threshold value: ' || l_tm_trtry_acct_thrshld);

      -- Truncate table SLS_RNK_ORG_TMP
      l_return_code   := usfdba.table_pkg.truncate_table(l_out_msg, 'XDMADM', 'SLS_RNK_ORG_TMP');
      DBMS_OUTPUT.put_line('Truncate table SLS_RNK_ORG_TMP return code: ' || l_return_code);

      DBMS_OUTPUT.put_line('Begin inserting SLS_RNK_ORG_TMP');

      INSERT /*+ append */
            INTO xdmadm.sls_rnk_org_tmp(fisc_yr_mth, org_lvl, org_val, rgn_cd, rgn_nm, area_cd,
                                        area_nm, grp_cd_or_brnch_cd, grp_nm_or_div_nm_cd_nbr, rgn_sls_mgr_id, dstrct_sls_mgr_id, trtry_mgr_id,
                                        brnch_trtry_cd, mth_st_elgbl, ytd_st_elgbl, mth_na_elgbl, ytd_na_elgbl, tm_st_cnt,
                                        tm_na_cnt, tm_st_cnt_ly, tm_na_cnt_ly)
         SELECT *
           FROM (WITH org_tmp AS
                        (  SELECT t.fisc_yr_mth,
                                  d.rgn_cd,
                                  d.rgn_nm,
                                  d.area_cd,
                                  d.area_nm,
                                  CASE WHEN d.grp_typ_cd = g_grp_typ_cd THEN 'G_' || d.grp_cd ELSE 'D_' || d.brnch_cd END grp_cd_or_brnch_cd,
                                  CASE WHEN d.grp_typ_cd = g_grp_typ_cd THEN d.grp_nm_or_div_nm_cd_nbr ELSE d.div_nm_cd_nbr END grp_nm_or_div_nm_cd_nbr,
                                  s.div_nbr,
                                  NVL(cc.rgn_sls_mgr_id, g_default_id) rgn_sls_mgr_id,
                                  NVL(cc.dstrct_sls_mgr_id, g_default_id) dstrct_sls_mgr_id,
                                  NVL(cc.trtry_mgr_id, g_default_id) trtry_mgr_id,
                                  d.brnch_cd || '-' || cc.trtry_cd brnch_trtry_cd,
                                  COUNT(DISTINCT CASE
                                                    WHEN cc.cust_natl_mngd_flg = 'L'
                                                     AND st.trtry_type = 'ST'
                                                     AND cc.pyr_seg_cd IN ('IND', 'REG', 'HOS', 'OTH') THEN
                                                       s.net_acct_cust_sk
                                                    ELSE
                                                       NULL
                                                 END)
                                     net_acct_st,
                                  COUNT(DISTINCT CASE
                                                    WHEN cc.cust_natl_mngd_flg = 'L'
                                                     AND st.trtry_type = 'ST'
                                                     AND cc.pyr_seg_cd IN ('IND', 'REG', 'HOS', 'OTH') THEN
                                                       s.net_acct_cust_sk_ly
                                                    ELSE
                                                       NULL
                                                 END)
                                     net_acct_st_ly,
                                  COUNT(DISTINCT CASE
                                                    WHEN cc.cust_natl_mngd_flg = 'N'
                                                     AND st.trtry_type = 'NA' THEN
                                                       s.net_acct_cust_sk
                                                    ELSE
                                                       NULL
                                                 END)
                                     net_acct_na,
                                  COUNT(DISTINCT CASE
                                                    WHEN cc.cust_natl_mngd_flg = 'N'
                                                     AND st.trtry_type = 'NA' THEN
                                                       s.net_acct_cust_sk_ly
                                                    ELSE
                                                       NULL
                                                 END)
                                     net_acct_na_ly
                             FROM xdmadm.cust_wk_aggr s,
                                  xdmadm.cust_corp cc,
                                  xdmadm.div_corp d,
                                  xdmadm.div_corp d2,
                                  xdmadm.wkly_time_corp t,
                                  xdmadm.sls_trtry_map st
                            WHERE s.cust_nbr = cc.cust_nbr
                              AND s.div_nbr = cc.div_nbr
                              AND s.div_nbr = d.div_nbr
                              AND s.sls_revenue_div_nbr = d2.div_nbr
                              AND s.fisc_yr_wk = t.fisc_yr_wk
                              AND cc.div_nbr = st.div_nbr
                              AND cc.trtry_cd = st.trtry_cd
                              AND t.fisc_yr = g_fisc_yr
                              AND t.fisc_yr_wk <= g_fisc_yr_wk
                              AND d2.div_typ_cd <> 'CC'
                              AND s.admn_flg = 'N'
                              AND cc.trd_cls = '1'
                              AND d.div_typ_cd = 'USF'
                              AND d.cmpny_cd = 1
                              AND NOT EXISTS (SELECT 'X'
                                                FROM xdmadm.param_value
                                               WHERE app_name = 'DIV_SALES_CHURN'
                                                 AND param_type = 'EXCLD_BRDLN_DIV'
                                                 AND param_value_nbr = d.div_nbr)
                         GROUP BY t.fisc_yr_mth,
                                  d.rgn_cd,
                                  d.rgn_nm,
                                  d.area_cd,
                                  d.area_nm,
                                  CASE WHEN d.grp_typ_cd = g_grp_typ_cd THEN 'G_' || d.grp_cd ELSE 'D_' || d.brnch_cd END,
                                  CASE WHEN d.grp_typ_cd = g_grp_typ_cd THEN d.grp_nm_or_div_nm_cd_nbr ELSE d.div_nm_cd_nbr END,
                                  s.div_nbr,
                                  cc.rgn_sls_mgr_id,
                                  NVL(cc.rgn_sls_mgr_id, g_default_id),
                                  NVL(cc.dstrct_sls_mgr_id, g_default_id),
                                  NVL(cc.trtry_mgr_id, g_default_id),
                                  d.brnch_cd || '-' || cc.trtry_cd)
                   SELECT g_fisc_yr_mth,
                          'TRTRY' org_lvl,
                          grp_cd_or_brnch_cd || '-' || rgn_sls_mgr_id || '-' || dstrct_sls_mgr_id || '-' || trtry_mgr_id || '-' || brnch_trtry_cd
                             org_val,
                          rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm,
                          grp_cd_or_brnch_cd,
                          grp_nm_or_div_nm_cd_nbr,
                          rgn_sls_mgr_id,
                          dstrct_sls_mgr_id,
                          trtry_mgr_id,
                          brnch_trtry_cd,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st_ly ELSE 0 END) >= l_tm_trtry_acct_thrshld
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st ELSE 0 END) >= 1 THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_st_elgbl,
                          CASE
                             WHEN MAX(net_acct_st_ly) >= l_tm_trtry_acct_thrshld
                              AND MAX(net_acct_st) >= 1 THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_st_elgbl,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na_ly ELSE 0 END) >= l_tm_trtry_acct_thrshld
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na ELSE 0 END) >= 1 THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_na_elgbl,
                          CASE
                             WHEN MAX(net_acct_na_ly) >= l_tm_trtry_acct_thrshld
                              AND MAX(net_acct_na) >= 1 THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_na_elgbl,
                          NULL tm_st_cnt,
                          NULL tm_na_cnt,
                          NULL tm_st_cnt_ly,
                          NULL tm_na_cnt_ly
                     FROM (  SELECT fisc_yr_mth,
                                    rgn_cd,
                                    rgn_nm,
                                    area_cd,
                                    area_nm,
                                    grp_cd_or_brnch_cd,
                                    grp_nm_or_div_nm_cd_nbr,
                                    rgn_sls_mgr_id,
                                    dstrct_sls_mgr_id,
                                    trtry_mgr_id,
                                    brnch_trtry_cd,
                                    SUM(net_acct_st) net_acct_st,
                                    SUM(net_acct_st_ly) net_acct_st_ly,
                                    SUM(net_acct_na) net_acct_na,
                                    SUM(net_acct_na_ly) net_acct_na_ly
                               FROM org_tmp
                           GROUP BY fisc_yr_mth,
                                    rgn_cd,
                                    rgn_nm,
                                    area_cd,
                                    area_nm,
                                    grp_cd_or_brnch_cd,
                                    grp_nm_or_div_nm_cd_nbr,
                                    rgn_sls_mgr_id,
                                    dstrct_sls_mgr_id,
                                    trtry_mgr_id,
                                    brnch_trtry_cd)
                 GROUP BY rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm,
                          grp_cd_or_brnch_cd,
                          grp_nm_or_div_nm_cd_nbr,
                          rgn_sls_mgr_id,
                          dstrct_sls_mgr_id,
                          trtry_mgr_id,
                          brnch_trtry_cd
                 UNION
                   SELECT g_fisc_yr_mth,
                          'TM' org_lvl,
                          grp_cd_or_brnch_cd || '-' || rgn_sls_mgr_id || '-' || dstrct_sls_mgr_id || '-' || trtry_mgr_id org_val,
                          rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm,
                          grp_cd_or_brnch_cd,
                          grp_nm_or_div_nm_cd_nbr,
                          rgn_sls_mgr_id,
                          dstrct_sls_mgr_id,
                          trtry_mgr_id,
                          NULL brnch_trtry_cd,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st_ly ELSE 0 END) >= l_tm_trtry_acct_thrshld
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st ELSE 0 END) >= 1 THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_st_elgbl,
                          CASE
                             WHEN MAX(net_acct_st_ly) >= l_tm_trtry_acct_thrshld
                              AND MAX(net_acct_st) >= 1 THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_st_elgbl,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na_ly ELSE 0 END) >= l_tm_trtry_acct_thrshld
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na ELSE 0 END) >= 1 THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_na_elgbl,
                          CASE
                             WHEN MAX(net_acct_na_ly) >= l_tm_trtry_acct_thrshld
                              AND MAX(net_acct_na) >= 1 THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_na_elgbl,
                          NULL tm_st_cnt,
                          NULL tm_na_cnt,
                          NULL tm_st_cnt_ly,
                          NULL tm_na_cnt_ly
                     FROM (  SELECT fisc_yr_mth,
                                    rgn_cd,
                                    rgn_nm,
                                    area_cd,
                                    area_nm,
                                    grp_cd_or_brnch_cd,
                                    grp_nm_or_div_nm_cd_nbr,
                                    rgn_sls_mgr_id,
                                    dstrct_sls_mgr_id,
                                    trtry_mgr_id,
                                    SUM(net_acct_st) net_acct_st,
                                    SUM(net_acct_st_ly) net_acct_st_ly,
                                    SUM(net_acct_na) net_acct_na,
                                    SUM(net_acct_na_ly) net_acct_na_ly
                               FROM org_tmp
                           GROUP BY fisc_yr_mth,
                                    rgn_cd,
                                    rgn_nm,
                                    area_cd,
                                    area_nm,
                                    grp_cd_or_brnch_cd,
                                    grp_nm_or_div_nm_cd_nbr,
                                    rgn_sls_mgr_id,
                                    dstrct_sls_mgr_id,
                                    trtry_mgr_id)
                 GROUP BY rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm,
                          grp_cd_or_brnch_cd,
                          grp_nm_or_div_nm_cd_nbr,
                          rgn_sls_mgr_id,
                          dstrct_sls_mgr_id,
                          trtry_mgr_id
                 UNION
                   SELECT g_fisc_yr_mth,
                          'DSM' org_lvl,
                          grp_cd_or_brnch_cd || '-' || rgn_sls_mgr_id || '-' || dstrct_sls_mgr_id org_val,
                          rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm,
                          grp_cd_or_brnch_cd,
                          grp_nm_or_div_nm_cd_nbr,
                          rgn_sls_mgr_id,
                          dstrct_sls_mgr_id,
                          NULL trtry_mgr_id,
                          NULL brnch_trtry_cd,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st_ly ELSE 0 END) >= l_rsm_dsm_acct_thrshld
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st ELSE 0 END) >= 1 THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_st_elgbl,
                          CASE
                             WHEN MAX(net_acct_st_ly) >= l_rsm_dsm_acct_thrshld
                              AND MAX(net_acct_st) >= 1 THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_st_elgbl,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na_ly ELSE 0 END) >= l_rsm_dsm_acct_thrshld
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na ELSE 0 END) >= 1 THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_na_elgbl,
                          CASE
                             WHEN MAX(net_acct_na_ly) >= l_rsm_dsm_acct_thrshld
                              AND MAX(net_acct_na) >= 1 THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_na_elgbl,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_st_cnt ELSE 0 END) tm_st_cnt,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_na_cnt ELSE 0 END) tm_na_cnt,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_st_cnt_ly ELSE 0 END) tm_st_cnt_ly,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_na_cnt_ly ELSE 0 END) tm_na_cnt_ly
                     FROM (  SELECT fisc_yr_mth,
                                    rgn_cd,
                                    rgn_nm,
                                    area_cd,
                                    area_nm,
                                    grp_cd_or_brnch_cd,
                                    grp_nm_or_div_nm_cd_nbr,
                                    rgn_sls_mgr_id,
                                    dstrct_sls_mgr_id,
                                    SUM(net_acct_st) net_acct_st,
                                    SUM(net_acct_st_ly) net_acct_st_ly,
                                    SUM(net_acct_na) net_acct_na,
                                    SUM(net_acct_na_ly) net_acct_na_ly,
                                    COUNT(DISTINCT CASE WHEN net_acct_st > 0 THEN trtry_mgr_id ELSE NULL END) tm_st_cnt,
                                    COUNT(DISTINCT CASE WHEN net_acct_na > 0 THEN trtry_mgr_id ELSE NULL END) tm_na_cnt,
                                    COUNT(DISTINCT CASE WHEN net_acct_st_ly > 0 THEN trtry_mgr_id ELSE NULL END) tm_st_cnt_ly,
                                    COUNT(DISTINCT CASE WHEN net_acct_na_ly > 0 THEN trtry_mgr_id ELSE NULL END) tm_na_cnt_ly
                               FROM org_tmp
                           GROUP BY fisc_yr_mth,
                                    rgn_cd,
                                    rgn_nm,
                                    area_cd,
                                    area_nm,
                                    grp_cd_or_brnch_cd,
                                    grp_nm_or_div_nm_cd_nbr,
                                    rgn_sls_mgr_id,
                                    dstrct_sls_mgr_id)
                 GROUP BY rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm,
                          grp_cd_or_brnch_cd,
                          grp_nm_or_div_nm_cd_nbr,
                          rgn_sls_mgr_id,
                          dstrct_sls_mgr_id
                 UNION
                   SELECT g_fisc_yr_mth,
                          'RSM' org_lvl,
                          grp_cd_or_brnch_cd || '-' || rgn_sls_mgr_id org_val,
                          rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm,
                          grp_cd_or_brnch_cd,
                          grp_nm_or_div_nm_cd_nbr,
                          rgn_sls_mgr_id,
                          NULL dstrct_sls_mgr_id,
                          NULL trtry_mgr_id,
                          NULL brnch_trtry_cd,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st_ly ELSE 0 END) >= l_rsm_dsm_acct_thrshld
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st ELSE 0 END) >= 1
                                  AND usr_flg = 'Y' THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_st_elgbl,
                          CASE
                             WHEN MAX(net_acct_st_ly) >= l_rsm_dsm_acct_thrshld
                              AND MAX(net_acct_st) >= 1
                              AND MAX(usr_flg) = 'Y' THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_st_elgbl,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na_ly ELSE 0 END) >= l_rsm_dsm_acct_thrshld
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na ELSE 0 END) >= 1
                                  AND usr_flg = 'Y' THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_na_elgbl,
                          CASE
                             WHEN MAX(net_acct_na_ly) >= l_rsm_dsm_acct_thrshld
                              AND MAX(net_acct_na) >= 1
                              AND MAX(usr_flg) = 'Y' THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_na_elgbl,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_st_cnt ELSE 0 END) tm_st_cnt,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_na_cnt ELSE 0 END) tm_na_cnt,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_st_cnt_ly ELSE 0 END) tm_st_cnt_ly,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_na_cnt_ly ELSE 0 END) tm_na_cnt_ly
                     FROM (SELECT fisc_yr_mth,
                                  rgn_cd,
                                  rgn_nm,
                                  area_cd,
                                  area_nm,
                                  grp_cd_or_brnch_cd,
                                  grp_nm_or_div_nm_cd_nbr,
                                  rgn_sls_mgr_id,
                                  net_acct_st,
                                  net_acct_st_ly,
                                  net_acct_na,
                                  net_acct_na_ly,
                                  tm_st_cnt,
                                  tm_na_cnt,
                                  tm_st_cnt_ly,
                                  tm_na_cnt_ly,
                                  NVL((SELECT DISTINCT 'Y'
                                         FROM xdmadm.obiee_user_ad_group
                                        WHERE username = a.rgn_sls_mgr_id),
                                      'N')
                                     usr_flg
                             FROM (  SELECT fisc_yr_mth,
                                            rgn_cd,
                                            rgn_nm,
                                            area_cd,
                                            area_nm,
                                            grp_cd_or_brnch_cd,
                                            grp_nm_or_div_nm_cd_nbr,
                                            rgn_sls_mgr_id,
                                            SUM(net_acct_st) net_acct_st,
                                            SUM(net_acct_st_ly) net_acct_st_ly,
                                            SUM(net_acct_na) net_acct_na,
                                            SUM(net_acct_na_ly) net_acct_na_ly,
                                            COUNT(DISTINCT CASE WHEN net_acct_st > 0 THEN trtry_mgr_id ELSE NULL END) tm_st_cnt,
                                            COUNT(DISTINCT CASE WHEN net_acct_na > 0 THEN trtry_mgr_id ELSE NULL END) tm_na_cnt,
                                            COUNT(DISTINCT CASE WHEN net_acct_st_ly > 0 THEN trtry_mgr_id ELSE NULL END) tm_st_cnt_ly,
                                            COUNT(DISTINCT CASE WHEN net_acct_na_ly > 0 THEN trtry_mgr_id ELSE NULL END) tm_na_cnt_ly
                                       FROM org_tmp
                                   GROUP BY fisc_yr_mth,
                                            rgn_cd,
                                            rgn_nm,
                                            area_cd,
                                            area_nm,
                                            grp_cd_or_brnch_cd,
                                            grp_nm_or_div_nm_cd_nbr,
                                            rgn_sls_mgr_id) a)
                 GROUP BY rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm,
                          grp_cd_or_brnch_cd,
                          grp_nm_or_div_nm_cd_nbr,
                          rgn_sls_mgr_id
                 UNION
                   SELECT g_fisc_yr_mth,
                          'GRP_DIV' org_lvl,
                          grp_cd_or_brnch_cd org_val,
                          rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm,
                          grp_cd_or_brnch_cd,
                          grp_nm_or_div_nm_cd_nbr,
                          NULL rgn_sls_mgr_id,
                          NULL dstrct_sls_mgr_id,
                          NULL trtry_mgr_id,
                          NULL brnch_trtry_cd,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st_ly ELSE 0 END) >= 1
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st ELSE 0 END) >= 1 THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_st_elgbl,
                          CASE
                             WHEN MAX(net_acct_st_ly) >= 1
                              AND MAX(net_acct_st) >= 1 THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_st_elgbl,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na_ly ELSE 0 END) >= 1
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na ELSE 0 END) >= 1 THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_na_elgbl,
                          CASE
                             WHEN MAX(net_acct_na_ly) >= 1
                              AND MAX(net_acct_na) >= 1 THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_na_elgbl,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_st_cnt ELSE 0 END) tm_st_cnt,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_na_cnt ELSE 0 END) tm_na_cnt,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_st_cnt_ly ELSE 0 END) tm_st_cnt_ly,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_na_cnt_ly ELSE 0 END) tm_na_cnt_ly
                     FROM (  SELECT fisc_yr_mth,
                                    rgn_cd,
                                    rgn_nm,
                                    area_cd,
                                    area_nm,
                                    grp_cd_or_brnch_cd,
                                    grp_nm_or_div_nm_cd_nbr,
                                    SUM(net_acct_st) net_acct_st,
                                    SUM(net_acct_st_ly) net_acct_st_ly,
                                    SUM(net_acct_na) net_acct_na,
                                    SUM(net_acct_na_ly) net_acct_na_ly,
                                    COUNT(DISTINCT CASE WHEN net_acct_st > 0 THEN trtry_mgr_id ELSE NULL END) tm_st_cnt,
                                    COUNT(DISTINCT CASE WHEN net_acct_na > 0 THEN trtry_mgr_id ELSE NULL END) tm_na_cnt,
                                    COUNT(DISTINCT CASE WHEN net_acct_st_ly > 0 THEN trtry_mgr_id ELSE NULL END) tm_st_cnt_ly,
                                    COUNT(DISTINCT CASE WHEN net_acct_na_ly > 0 THEN trtry_mgr_id ELSE NULL END) tm_na_cnt_ly
                               FROM org_tmp
                           GROUP BY fisc_yr_mth,
                                    rgn_cd,
                                    rgn_nm,
                                    area_cd,
                                    area_nm,
                                    grp_cd_or_brnch_cd,
                                    grp_nm_or_div_nm_cd_nbr)
                 GROUP BY rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm,
                          grp_cd_or_brnch_cd,
                          grp_nm_or_div_nm_cd_nbr
                 UNION
                   SELECT g_fisc_yr_mth,
                          'AREA' org_lvl,
                          area_cd org_val,
                          rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm,
                          NULL grp_cd_or_brnch_cd,
                          NULL grp_nm_or_div_nm_cd_nbr,
                          NULL rgn_sls_mgr_id,
                          NULL dstrct_sls_mgr_id,
                          NULL trtry_mgr_id,
                          NULL brnch_trtry_cd,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st_ly ELSE 0 END) >= 1
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_st ELSE 0 END) >= 1 THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_st_elgbl,
                          CASE
                             WHEN MAX(net_acct_st_ly) >= 1
                              AND MAX(net_acct_st) >= 1 THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_st_elgbl,
                          MAX(CASE
                                 WHEN (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na_ly ELSE 0 END) >= 1
                                  AND (CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN net_acct_na ELSE 0 END) >= 1 THEN
                                    'Y'
                                 ELSE
                                    'N'
                              END)
                             mth_na_elgbl,
                          CASE
                             WHEN MAX(net_acct_na_ly) >= 1
                              AND MAX(net_acct_na) >= 1 THEN
                                'Y'
                             ELSE
                                'N'
                          END
                             ytd_na_elgbl,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_st_cnt ELSE 0 END) tm_st_cnt,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_na_cnt ELSE 0 END) tm_na_cnt,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_st_cnt_ly ELSE 0 END) tm_st_cnt_ly,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_na_cnt_ly ELSE 0 END) tm_na_cnt_ly
                     FROM (  SELECT fisc_yr_mth,
                                    rgn_cd,
                                    rgn_nm,
                                    area_cd,
                                    area_nm,
                                    SUM(net_acct_st) net_acct_st,
                                    SUM(net_acct_st_ly) net_acct_st_ly,
                                    SUM(net_acct_na) net_acct_na,
                                    SUM(net_acct_na_ly) net_acct_na_ly,
                                    COUNT(DISTINCT CASE WHEN net_acct_st > 0 THEN trtry_mgr_id ELSE NULL END) tm_st_cnt,
                                    COUNT(DISTINCT CASE WHEN net_acct_na > 0 THEN trtry_mgr_id ELSE NULL END) tm_na_cnt,
                                    COUNT(DISTINCT CASE WHEN net_acct_st_ly > 0 THEN trtry_mgr_id ELSE NULL END) tm_st_cnt_ly,
                                    COUNT(DISTINCT CASE WHEN net_acct_na_ly > 0 THEN trtry_mgr_id ELSE NULL END) tm_na_cnt_ly
                               FROM org_tmp
                           GROUP BY fisc_yr_mth,
                                    rgn_cd,
                                    rgn_nm,
                                    area_cd,
                                    area_nm)
                 GROUP BY rgn_cd,
                          rgn_nm,
                          area_cd,
                          area_nm
                 UNION
                   SELECT g_fisc_yr_mth,
                          'RGN' org_lvl,
                          rgn_cd org_val,
                          rgn_cd,
                          rgn_nm,
                          NULL area_cd,
                          NULL area_nm,
                          NULL grp_cd_or_brnch_cd,
                          NULL grp_nm_or_div_nm_cd_nbr,
                          NULL rgn_sls_mgr_id,
                          NULL dstrct_sls_mgr_id,
                          NULL trtry_mgr_id,
                          NULL brnch_trtry_cd,
                          'Y' mth_st_elgbl,
                          'Y' ytd_st_elgbl,
                          'Y' mth_na_elgbl,
                          'Y' ytd_na_elgbl,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_st_cnt ELSE 0 END) tm_st_cnt,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_na_cnt ELSE 0 END) tm_na_cnt,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_st_cnt_ly ELSE 0 END) tm_st_cnt_ly,
                          SUM(CASE WHEN fisc_yr_mth = g_fisc_yr_mth THEN tm_na_cnt_ly ELSE 0 END) tm_na_cnt_ly
                     FROM (  SELECT fisc_yr_mth, rgn_cd, rgn_nm, COUNT(DISTINCT CASE WHEN net_acct_st > 0 THEN trtry_mgr_id ELSE NULL END) tm_st_cnt, COUNT(DISTINCT CASE WHEN net_acct_na > 0 THEN trtry_mgr_id ELSE NULL END) tm_na_cnt, COUNT(DISTINCT CASE WHEN net_acct_st_ly > 0 THEN trtry_mgr_id ELSE NULL END) tm_st_cnt_ly, COUNT(DISTINCT CASE WHEN net_acct_na_ly > 0 THEN trtry_mgr_id ELSE NULL END) tm_na_cnt_ly
                               FROM org_tmp
                           GROUP BY fisc_yr_mth, rgn_cd, rgn_nm)
                 GROUP BY rgn_cd, rgn_nm);

      DBMS_OUTPUT.put_line('Inserted ' || sql%ROWCOUNT || ' rows');

      COMMIT;

      -- Get stats on SLS_RNK_ORG_TMP table
      l_return_code   := xdmadm.fn_getstat_tbl('XDMADM', 'SLS_RNK_ORG_TMP');
      DBMS_OUTPUT.put_line('Analyze SLS_RNK_ORG_TMP Return Code: ' || l_return_code);

      IF l_return_code <> 0 THEN
         DBMS_OUTPUT.put_line('Error: Failed to analyze table SLS_RNK_ORG_TMP');
      END IF;
   END ins_sls_rnk_org;

   /***********************************************************************
   * Name: INS_SLS_RNK_TMP
   * Type: Procedure
   * Description: Populates SLS_RNK_TMP table with aggregate data used to build the
   *              ranking table.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        06/05/2013  Matt Nicol       Initial Creation
   ************************************************************************/
   PROCEDURE ins_sls_rnk_tmp IS
      l_return_code   NUMBER := 0;
      l_out_msg       VARCHAR2(256);
   BEGIN
      -- Truncate table SLS_RNK_TMP
      l_return_code   := usfdba.table_pkg.truncate_table(l_out_msg, 'XDMADM', 'SLS_RNK_TMP');
      DBMS_OUTPUT.put_line('Truncate table SLS_RNK_TMP return code: ' || l_return_code);

      DBMS_OUTPUT.put_line('Begin inserting SLS_RNK_TMP');

      INSERT INTO xdmadm.sls_rnk_tmp
         (  SELECT g_fisc_yr_mth fisc_yr_mth,
                   d.rgn_cd,
                   CASE WHEN d.grp_typ_cd = g_grp_typ_cd THEN 'G_' || d.grp_cd ELSE 'D_' || d.brnch_cd END grp_cd_or_brnch_cd,
                   s.div_nbr,
                   NVL(cc.rgn_sls_mgr_id, g_default_id) rgn_sls_mgr_id,
                   NVL(cc.dstrct_sls_mgr_id, g_default_id) dstrct_sls_mgr_id,
                   NVL(cc.trtry_mgr_id, g_default_id) trtry_mgr_id,
                   d.brnch_cd || '-' || cc.trtry_cd brnch_trtry_cd,
                   cc.pyr_seg_cd,
                   cc.cust_natl_mngd_flg,
                   st.trtry_type,
                   cls.cust_clsfctn,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.grs_sls_extnd, 0) ELSE 0 END) grs_sls_extnd,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.grs_sls_extnd_ly, 0) ELSE 0 END) grs_sls_extnd_ly,
                   SUM(NVL(s.grs_sls_extnd, 0)) grs_sls_extnd_ytd,
                   SUM(NVL(s.grs_sls_extnd_ly, 0)) grs_sls_extnd_ytd_ly,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.unfrm_qty_ship, 0) ELSE 0 END) unfrm_qty_ship,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.unfrm_qty_ship_ly, 0) ELSE 0 END) unfrm_qty_ship_ly,
                   SUM(NVL(s.unfrm_qty_ship, 0)) unfrm_qty_ship_ytd,
                   SUM(NVL(s.unfrm_qty_ship_ly, 0)) unfrm_qty_ship_ytd_ly,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.grs_tgp, 0) ELSE 0 END) grs_tgp,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.grs_tgp_ly, 0) ELSE 0 END) grs_tgp_ly,
                   SUM(NVL(s.grs_tgp, 0)) grs_tgp_ytd,
                   SUM(NVL(s.grs_tgp_ly, 0)) grs_tgp_ytd_ly,
                   COUNT(DISTINCT CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN s.net_acct_cust_sk ELSE NULL END) net_acct,
                   COUNT(DISTINCT CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN s.net_acct_cust_sk_ly ELSE NULL END) net_acct_ly,
                   COUNT(DISTINCT s.net_acct_cust_sk) net_acct_ytd,
                   COUNT(DISTINCT s.net_acct_cust_sk_ly) net_acct_ytd_ly,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.rt_ln_cnt, 0) ELSE 0 END) rt_ln_cnt,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.vndr_ship_ln_cnt, 0) ELSE 0 END) vndr_ship_ln_cnt,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.wc_ln_cnt, 0) ELSE 0 END) wc_ln_cnt,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.drop_cnt, 0) ELSE 0 END) drop_cnt,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.rt_ln_cnt_ly, 0) ELSE 0 END) rt_ln_cnt_ly,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.vndr_ship_ln_cnt_ly, 0) ELSE 0 END) vndr_ship_ln_cnt_ly,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.wc_ln_cnt_ly, 0) ELSE 0 END) wc_ln_cnt_ly,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.drop_cnt_ly, 0) ELSE 0 END) drop_cnt_ly,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.eb_grs_sls_extnd, 0) ELSE 0 END) eb_grs_sls_extnd,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.eb_grs_sls_extnd_ly, 0) ELSE 0 END) eb_grs_sls_extnd_ly,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.ec_grs_sls_extnd, 0) ELSE 0 END) ec_grs_sls_extnd,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.rt_grs_sls_extnd, 0) ELSE 0 END) rt_grs_sls_extnd,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.vs_grs_sls_extnd, 0) ELSE 0 END) vs_grs_sls_extnd,
                   SUM(CASE WHEN t.fisc_yr_mth = g_fisc_yr_mth THEN NVL(s.wc_grs_sls_extnd, 0) ELSE 0 END) wc_grs_sls_extnd,
                   SUM(CASE
                          WHEN t.fisc_yr_mth = g_fisc_yr_mth
                           AND pyr.usf_top_pyr_seg_cd = 1 THEN
                             NVL(s.grs_sls_extnd, 0)
                          ELSE
                             0
                       END)
                      ir_grs_sls_extnd,
                   SUM(CASE WHEN pyr.usf_top_pyr_seg_cd = 1 THEN NVL(s.grs_sls_extnd, 0) ELSE 0 END) ir_grs_sls_extnd_ytd,
                   d.area_cd
              FROM xdmadm.cust_wk_aggr s,
                   xdmadm.cust_corp cc,
                   xdmadm.cust_clsfctn_corp cls,
                   xdmadm.div_corp d,
                   xdmadm.div_corp d2,
                   xdmadm.wkly_time_corp t,
                   xdmadm.sls_trtry_map st,
                   xdmadm.cust_pyr_seg_map pyr
             WHERE s.cust_nbr = cc.cust_nbr
               AND s.div_nbr = cc.div_nbr
               AND s.cust_nbr = cls.cust_nbr
               AND s.div_nbr = cls.div_nbr
               AND t.fisc_yr_mth = cls.fisc_yr_mth
               AND s.div_nbr = d.div_nbr
               AND s.fisc_yr_wk = t.fisc_yr_wk
               AND cc.div_nbr = st.div_nbr
               AND cc.trtry_cd = st.trtry_cd
               AND cc.pyr_seg_cd = pyr.pyr_seg_cd
               -- AND s.sls_revenue_div_nbr = d2.div_nbr
               AND t.fisc_yr = g_fisc_yr
               AND t.fisc_yr_wk <= g_fisc_yr_wk
               AND s.admn_flg = 'N'
               AND cc.trd_cls = '1'
               AND d.div_typ_cd = 'USF'
               AND d2.div_typ_cd <> 'CC'
               AND NVL(d2.acqstn_cmpny_nm, 'USF') = 'USF' --Modified for Acquisition
               AND NVL(s.sls_revenue_div_nbr, d2.div_nbr) = d2.div_nbr --Modified for Acquisition
               AND d.cmpny_cd = 1
               AND NOT EXISTS (SELECT 'X'
                                 FROM xdmadm.param_value
                                WHERE app_name = 'DIV_SALES_CHURN'
                                  AND param_type = 'EXCLD_BRDLN_DIV'
                                  AND param_value_nbr = d.div_nbr)
          GROUP BY d.rgn_cd,
                   CASE WHEN d.grp_typ_cd = g_grp_typ_cd THEN 'G_' || d.grp_cd ELSE 'D_' || d.brnch_cd END,
                   s.div_nbr,
                   cc.rgn_sls_mgr_id,
                   cc.dstrct_sls_mgr_id,
                   cc.trtry_mgr_id,
                   d.brnch_cd || '-' || cc.trtry_cd,
                   cc.pyr_seg_cd,
                   cc.cust_natl_mngd_flg,
                   st.trtry_type,
                   cls.cust_clsfctn,
                   d.area_cd);

      DBMS_OUTPUT.put_line('Inserted ' || sql%ROWCOUNT || ' rows in SLS_RNK_TMP table');

      COMMIT;

      -- Get stats on SLS_RNK_TMP table
      l_return_code   := xdmadm.fn_getstat_tbl('XDMADM', 'SLS_RNK_TMP');
      DBMS_OUTPUT.put_line('Analyze SLS_RNK_TMP Return Code: ' || l_return_code);

      IF l_return_code <> 0 THEN
         DBMS_OUTPUT.put_line('Error: Failed to analyze table SLS_RNK_TMP');
      END IF;
   END ins_sls_rnk_tmp;

   /***********************************************************************
   * Name: INS_SLS_RNK_MTH
   * Type: Procedure
   * Description: Populates SLS_RNK_MTH_TMP table with measures but no
                  ranking or percentile columns.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        06/05/2013  Matt Nicol       Initial Creation
   ************************************************************************/
   PROCEDURE ins_sls_rnk_mth IS
      -- Cursor used loop through each org level and dynamically create corresponding joins
      CURSOR c_org_lvl IS
         SELECT param_code org_lvl,
                CASE param_code
                   WHEN 'TRTRY' THEN
                      'tmp.grp_cd_or_brnch_cd = org.grp_cd_or_brnch_cd
                    AND tmp.rgn_sls_mgr_id = org.rgn_sls_mgr_id
                    AND tmp.dstrct_sls_mgr_id = org.dstrct_sls_mgr_id
                    AND tmp.trtry_mgr_id = org.trtry_mgr_id
                    AND tmp.brnch_trtry_cd = org.brnch_trtry_cd'
                   WHEN 'TM' THEN
                      'tmp.grp_cd_or_brnch_cd = org.grp_cd_or_brnch_cd
                    AND tmp.rgn_sls_mgr_id = org.rgn_sls_mgr_id
                    AND tmp.dstrct_sls_mgr_id = org.dstrct_sls_mgr_id
                    AND tmp.trtry_mgr_id = org.trtry_mgr_id'
                   WHEN 'DSM' THEN
                      'tmp.grp_cd_or_brnch_cd = org.grp_cd_or_brnch_cd
                    AND tmp.rgn_sls_mgr_id = org.rgn_sls_mgr_id
                    AND tmp.dstrct_sls_mgr_id = org.dstrct_sls_mgr_id'
                   WHEN 'RSM' THEN
                      'tmp.grp_cd_or_brnch_cd = org.grp_cd_or_brnch_cd
                    AND tmp.rgn_sls_mgr_id = org.rgn_sls_mgr_id'
                   WHEN 'GRP_DIV' THEN
                      'tmp.grp_cd_or_brnch_cd = org.grp_cd_or_brnch_cd'
                   WHEN 'AREA' THEN
                      'tmp.area_cd = org.area_cd'
                   WHEN 'RGN' THEN
                      'tmp.rgn_cd = org.rgn_cd'
                END
                   predicate
           FROM xdmadm.param_value
          WHERE app_name = 'SLS_RNK'
            AND param_type = 'ORG_LVL';

      -- Cursor used loop through rank categories and dynamically create filters
      -- Rank filter is used to filter measures being populated in TABLE
      -- Population filter is used to build list of org entities begin populated in TABLES
      -- Category 0 is used to get IR percent values with no ranking category filter
      CURSOR c_rnk_ctgry IS
         SELECT rnk_ctgry,
                CASE rnk_ctgry
                   WHEN 1 THEN 'cust_natl_mngd_flg = ''L'' AND trtry_type = ''ST'''
                   WHEN 2 THEN 'cust_natl_mngd_flg = ''L'' AND trtry_type = ''ST'' AND pyr_seg_cd = ''IND'''
                   WHEN 3 THEN 'cust_natl_mngd_flg = ''L'' AND trtry_type = ''ST'' AND pyr_seg_cd IN (''IND'', ''REG'', ''HOS'', ''OTH'')'
                   WHEN 4 THEN 'cust_natl_mngd_flg = ''N'' AND trtry_type = ''NA'''
                   WHEN 5 THEN 'cust_natl_mngd_flg = ''N'' AND pyr_seg_cd = ''HOS'' AND trtry_type = ''NA'''
                   WHEN 6 THEN 'cust_natl_mngd_flg = ''N'' AND pyr_seg_cd = ''REG'' AND trtry_type = ''NA'''
                END
                   rnk_predicate,
                CASE rnk_ctgry
                   WHEN 1 THEN 'cust_natl_mngd_flg = ''L'' AND trtry_type = ''ST'' AND pyr_seg_cd IN (''IND'', ''REG'', ''HOS'', ''OTH'')'
                   WHEN 2 THEN 'cust_natl_mngd_flg = ''L'' AND trtry_type = ''ST'' AND pyr_seg_cd IN (''IND'', ''REG'', ''HOS'', ''OTH'')'
                   WHEN 3 THEN 'cust_natl_mngd_flg = ''L'' AND trtry_type = ''ST'' AND pyr_seg_cd IN (''IND'', ''REG'', ''HOS'', ''OTH'')'
                   WHEN 4 THEN 'cust_natl_mngd_flg = ''N'' AND trtry_type = ''NA'''
                   WHEN 5 THEN 'cust_natl_mngd_flg = ''N'' AND trtry_type = ''NA'''
                   WHEN 6 THEN 'cust_natl_mngd_flg = ''N'' AND trtry_type = ''NA'''
                END
                   pop_predicate
           FROM xdmadm.sls_rnk_ctgry
         UNION
         SELECT 0, '1=1', '1=1' FROM DUAL;

      -- Cursor used loop through customer classifications and dynamically create filters
      CURSOR c_cust_clsfctn IS
         SELECT param_code cust_clsfctn, CASE param_code WHEN 'ALL' THEN '1=1' ELSE 'CUST_CLSFCTN = ''' || param_code || '''' END predicate
           FROM xdmadm.param_value
          WHERE app_name = 'SLS_RNK'
            AND param_type = 'CUST_CLSFCTN_LOV';

      l_sql           VARCHAR2(32000);
      l_return_code   NUMBER := 0;
      l_out_msg       VARCHAR2(256);
   BEGIN
      -- Truncate table XDMADM.SLS_RNK_MTH_TMP
      l_return_code   := usfdba.table_pkg.truncate_table(l_out_msg, 'XDMADM', 'SLS_RNK_MTH_TMP');
      DBMS_OUTPUT.put_line('Truncate table SLS_RNK_MTH_TMP return code: ' || l_return_code);

      DBMS_OUTPUT.put_line('Begin inserting SLS_RNK_MTH_TMP');

      FOR r_org_lvl IN c_org_lvl LOOP
         FOR r_rnk_ctgry IN c_rnk_ctgry LOOP
            FOR r_cust_clsfctn IN c_cust_clsfctn LOOP
               l_sql   :=
                  'INSERT /*+ append */ INTO xdmadm.sls_rnk_mth_tmp(fisc_yr_mth, org_lvl, org_val, rnk_ctgry, rnk_lvl, cust_clsfctn,
                                   rnk_by, grp_cd_or_brnch_cd, ec_pct, ir_pct, ir_pct_ytd, grs_sls_extnd,
                                   grs_sls_extnd_ly, grs_sls_extnd_var, grs_sls_extnd_ytd, grs_sls_extnd_ytd_ly, grs_sls_extnd_ytd_var, unfrm_qty_ship,
                                   unfrm_qty_ship_ly, unfrm_qty_ship_var, unfrm_qty_ship_ytd, unfrm_qty_ship_ytd_ly, unfrm_qty_ship_ytd_var, grs_tgp,
                                   grs_tgp_ly, grs_tgp_var, grs_tgp_ytd, grs_tgp_ytd_ly, grs_tgp_ytd_var, net_acct,
                                   net_acct_ly, net_acct_var, net_acct_per_tm, net_acct_per_tm_ly, net_acct_per_tm_var, lines_per_drop,
                                   lines_per_drop_ly, lines_per_drop_var, eb_pct, eb_pct_ly, eb_pct_var)
         SELECT *
           FROM (WITH pop AS (SELECT DISTINCT org.org_val, org.grp_cd_or_brnch_cd
                                FROM xdmadm.sls_rnk_tmp tmp, xdmadm.sls_rnk_org_tmp org
                               WHERE '
                  || r_org_lvl.predicate
                  || '
                                 AND '
                  || r_rnk_ctgry.pop_predicate
                  || '
                                 AND org.org_lvl = '''
                  || r_org_lvl.org_lvl
                  || '''
                                 AND org.fisc_yr_mth = '''
                  || g_fisc_yr_mth
                  || '''),
                     rnk AS
                        (  SELECT org.org_val,
                               CASE
                                  WHEN SUM(tmp.rt_grs_sls_extnd + tmp.vs_grs_sls_extnd + tmp.wc_grs_sls_extnd) > 0 THEN
                                    (SUM(tmp.ec_grs_sls_extnd) / SUM(tmp.rt_grs_sls_extnd + tmp.vs_grs_sls_extnd + tmp.wc_grs_sls_extnd)) * 100
                                  ELSE 0
                               END
                                  ec_pct,
                               CASE
                                  WHEN SUM(tmp.grs_sls_extnd) > 0 THEN (SUM(tmp.ir_grs_sls_extnd) / SUM(tmp.grs_sls_extnd)) * 100
                                  ELSE 0
                               END
                                  ir_pct,
                               CASE
                                  WHEN SUM(tmp.grs_sls_extnd_ytd) > 0 THEN
                                     (SUM(tmp.ir_grs_sls_extnd_ytd) / SUM(tmp.grs_sls_extnd_ytd)) * 100
                                  ELSE
                                     0
                               END
                                  ir_pct_ytd,
                               SUM(tmp.grs_sls_extnd) grs_sls_extnd,
                               SUM(tmp.grs_sls_extnd_ly) grs_sls_extnd_ly,
                               SUM(tmp.grs_sls_extnd) - SUM(tmp.grs_sls_extnd_ly) grs_sls_extnd_var,
                               SUM(tmp.grs_sls_extnd_ytd) grs_sls_extnd_ytd,
                               SUM(tmp.grs_sls_extnd_ytd_ly) grs_sls_extnd_ytd_ly,
                               SUM(tmp.grs_sls_extnd_ytd) - SUM(tmp.grs_sls_extnd_ytd_ly) grs_sls_extnd_ytd_var,
                               SUM(tmp.unfrm_qty_ship) unfrm_qty_ship,
                               SUM(tmp.unfrm_qty_ship_ly) unfrm_qty_ship_ly,
                               SUM(tmp.unfrm_qty_ship) - SUM(tmp.unfrm_qty_ship_ly) unfrm_qty_ship_var,
                               SUM(tmp.unfrm_qty_ship_ytd) unfrm_qty_ship_ytd,
                               SUM(tmp.unfrm_qty_ship_ytd_ly) unfrm_qty_ship_ytd_ly,
                               SUM(tmp.unfrm_qty_ship_ytd) - SUM(tmp.unfrm_qty_ship_ytd_ly) unfrm_qty_ship_ytd_var,
                               SUM(tmp.grs_tgp) grs_tgp,
                               SUM(tmp.grs_tgp_ly) grs_tgp_ly,
                               SUM(tmp.grs_tgp) - SUM(tmp.grs_tgp_ly) grs_tgp_var,
                               SUM(tmp.grs_tgp_ytd) grs_tgp_ytd,
                               SUM(tmp.grs_tgp_ytd_ly) grs_tgp_ytd_ly,
                               SUM(tmp.grs_tgp_ytd) - SUM(tmp.grs_tgp_ytd_ly) grs_tgp_ytd_var,
                               SUM(tmp.net_acct) net_acct,
                               SUM(tmp.net_acct_ly) net_acct_ly,
                               SUM(tmp.net_acct) - SUM(tmp.net_acct_ly) net_acct_var,
                               CASE
                                  WHEN org.tm_cnt > 0 THEN
                                     SUM(tmp.net_acct) / org.tm_cnt
                                  ELSE
                                     0
                               END
                                  net_acct_per_tm,
                               CASE
                                  WHEN org.tm_cnt_ly > 0 THEN
                                     SUM(tmp.net_acct_ly) / org.tm_cnt_ly
                                  ELSE
                                     0
                               END
                                  net_acct_per_tm_ly,
                              CASE
                                  WHEN org.tm_cnt > 0 THEN
                                     SUM(tmp.net_acct) / org.tm_cnt
                                  ELSE
                                     0
                               END
                                  -
                               CASE
                                  WHEN org.tm_cnt_ly > 0 THEN
                                     SUM(tmp.net_acct_ly) / org.tm_cnt_ly
                                  ELSE
                                     0
                               END  net_acct_per_tm_var,
                               CASE
                                  WHEN SUM(tmp.drop_cnt) > 0 THEN
                                     SUM(tmp.rt_ln_cnt + tmp.vndr_ship_ln_cnt + tmp.wc_ln_cnt) / SUM(tmp.drop_cnt)
                                  ELSE
                                     0
                               END
                                  lines_per_drop,
                               CASE
                                  WHEN SUM(tmp.drop_cnt_ly) > 0 THEN
                                     SUM(tmp.rt_ln_cnt_ly + tmp.vndr_ship_ln_cnt_ly + tmp.wc_ln_cnt_ly) / SUM(tmp.drop_cnt_ly)
                                  ELSE
                                     0
                               END
                                  lines_per_drop_ly,
                               CASE
                                  WHEN SUM(tmp.drop_cnt) > 0 THEN
                                     SUM(tmp.rt_ln_cnt + tmp.vndr_ship_ln_cnt + tmp.wc_ln_cnt) / SUM(tmp.drop_cnt)
                                  ELSE
                                     0
                               END
                               - CASE
                                    WHEN SUM(tmp.drop_cnt_ly) > 0 THEN
                                       SUM(tmp.rt_ln_cnt_ly + tmp.vndr_ship_ln_cnt_ly + tmp.wc_ln_cnt_ly) / SUM(tmp.drop_cnt_ly)
                                    ELSE
                                       0
                                 END
                                  lines_per_drop_var,
                               CASE
                                  WHEN SUM(tmp.grs_sls_extnd) > 0 THEN (SUM(tmp.eb_grs_sls_extnd) / SUM(tmp.grs_sls_extnd)) * 100
                                  ELSE 0
                               END
                                  eb_pct,
                               CASE
                                  WHEN SUM(tmp.grs_sls_extnd_ly) > 0 THEN (SUM(tmp.eb_grs_sls_extnd_ly) / SUM(tmp.grs_sls_extnd_ly)) * 100
                                  ELSE 0
                               END
                                  eb_pct_ly,
                               CASE
                                  WHEN SUM(tmp.grs_sls_extnd) > 0 THEN (SUM(tmp.eb_grs_sls_extnd) / SUM(tmp.grs_sls_extnd)) * 100
                                  ELSE 0
                               END
                               - CASE
                                    WHEN SUM(tmp.grs_sls_extnd_ly) > 0 THEN
                                       (SUM(tmp.eb_grs_sls_extnd_ly) / SUM(tmp.grs_sls_extnd_ly)) * 100
                                    ELSE
                                       0
                                 END
                                  eb_pct_var
                             FROM (SELECT *
                                     FROM xdmadm.sls_rnk_tmp
                                    WHERE '
                  || r_rnk_ctgry.rnk_predicate
                  || '
                                      AND '
                  || r_cust_clsfctn.predicate
                  || ') tmp,
                            (SELECT o.*, CASE WHEN '
                  || r_rnk_ctgry.rnk_ctgry
                  || ' <= 3 THEN tm_st_cnt ELSE tm_na_cnt END tm_cnt,
                                         CASE WHEN '
                  || r_rnk_ctgry.rnk_ctgry
                  || ' <= 3 THEN tm_st_cnt_ly ELSE tm_na_cnt_ly END tm_cnt_ly
                               FROM xdmadm.sls_rnk_org_tmp o) org
                            WHERE '
                  || r_org_lvl.predicate
                  || '
                              AND org.org_lvl = '''
                  || r_org_lvl.org_lvl
                  || '''
                              AND org.fisc_yr_mth = '''
                  || g_fisc_yr_mth
                  || '''
                         GROUP BY org.org_val, org.tm_cnt, org.tm_cnt_ly)
                 SELECT '''
                  || g_fisc_yr_mth
                  || ''' fisc_yr_mth,
                        '''
                  || r_org_lvl.org_lvl
                  || ''' org_lvl,
                        pop.org_val, '
                  || r_rnk_ctgry.rnk_ctgry
                  || ' rnk_ctgry,
                        pv.param_code rnk_lvl,
                        '''
                  || r_cust_clsfctn.cust_clsfctn
                  || ''' cust_clsfctn,
                        ''VAR'' rnk_by,
                        pop.grp_cd_or_brnch_cd,
                        rnk.ec_pct,
                        rnk.ir_pct,
                        rnk.ir_pct_ytd,
                        rnk.grs_sls_extnd,
                        rnk.grs_sls_extnd_ly,
                        rnk.grs_sls_extnd_var,
                        rnk.grs_sls_extnd_ytd,
                        rnk.grs_sls_extnd_ytd_ly,
                        rnk.grs_sls_extnd_ytd_var,
                        rnk.unfrm_qty_ship,
                        rnk.unfrm_qty_ship_ly,
                        rnk.unfrm_qty_ship_var,
                        rnk.unfrm_qty_ship_ytd,
                        rnk.unfrm_qty_ship_ytd_ly,
                        rnk.unfrm_qty_ship_ytd_var,
                        rnk.grs_tgp,
                        rnk.grs_tgp_ly,
                        rnk.grs_tgp_var,
                        rnk.grs_tgp_ytd,
                        rnk.grs_tgp_ytd_ly,
                        rnk.grs_tgp_ytd_var,
                        rnk.net_acct,
                        rnk.net_acct_ly,
                        rnk.net_acct_var,
                        rnk.net_acct_per_tm,
                        rnk.net_acct_per_tm_ly,
                        rnk.net_acct_per_tm_var,
                        rnk.lines_per_drop,
                        rnk.lines_per_drop_ly,
                        rnk.lines_per_drop_var,
                        rnk.eb_pct,
                        rnk.eb_pct_ly,
                        rnk.eb_pct_var
                   FROM pop, rnk, xdmadm.param_value pv
                  WHERE pop.org_val = rnk.org_val(+)
                    AND pv.app_name = ''SLS_RNK''
                    AND pv.param_type = ''RNK_LVL_LOV'')';

               EXECUTE IMMEDIATE l_sql;

               COMMIT;

               l_sql   :=
                  'INSERT /*+ append */ INTO xdmadm.sls_rnk_mth_tmp(fisc_yr_mth, org_lvl, org_val, rnk_ctgry, rnk_lvl, cust_clsfctn,
                                   rnk_by, grp_cd_or_brnch_cd, ec_pct, ir_pct, ir_pct_ytd, grs_sls_extnd,
                                   grs_sls_extnd_ly, grs_sls_extnd_var, grs_sls_extnd_ytd, grs_sls_extnd_ytd_ly, grs_sls_extnd_ytd_var, unfrm_qty_ship,
                                   unfrm_qty_ship_ly, unfrm_qty_ship_var, unfrm_qty_ship_ytd, unfrm_qty_ship_ytd_ly, unfrm_qty_ship_ytd_var, grs_tgp,
                                   grs_tgp_ly, grs_tgp_var, grs_tgp_ytd, grs_tgp_ytd_ly, grs_tgp_ytd_var, net_acct,
                                   net_acct_ly, net_acct_var, net_acct_per_tm, net_acct_per_tm_ly, net_acct_per_tm_var, lines_per_drop,
                                   lines_per_drop_ly, lines_per_drop_var, eb_pct, eb_pct_ly, eb_pct_var)
         SELECT *
           FROM (WITH pop AS (SELECT DISTINCT org.org_val, org.grp_cd_or_brnch_cd
                                FROM xdmadm.sls_rnk_tmp tmp, xdmadm.sls_rnk_org_tmp org
                               WHERE '
                  || r_org_lvl.predicate
                  || '
                                 AND '
                  || r_rnk_ctgry.pop_predicate
                  || '
                                 AND org.org_lvl = '''
                  || r_org_lvl.org_lvl
                  || '''
                                 AND org.fisc_yr_mth = '''
                  || g_fisc_yr_mth
                  || '''),
                     rnk AS
                        (  SELECT org.org_val,
                                  CASE
                                     WHEN SUM(tmp.rt_grs_sls_extnd + tmp.vs_grs_sls_extnd + tmp.wc_grs_sls_extnd) > 0 THEN
                                        (SUM(tmp.ec_grs_sls_extnd) / SUM(tmp.rt_grs_sls_extnd + tmp.vs_grs_sls_extnd + tmp.wc_grs_sls_extnd)) * 100
                                     ELSE
                                        0
                                  END
                                     ec_pct,
                                  CASE
                                     WHEN SUM(tmp.grs_sls_extnd) > 0 THEN
                                        (SUM(tmp.ir_grs_sls_extnd) / SUM(tmp.grs_sls_extnd)) * 100
                                     ELSE
                                        0
                                  END
                                     ir_pct,
                                  CASE
                                     WHEN SUM(tmp.grs_sls_extnd_ytd) > 0 THEN
                                        (SUM(tmp.ir_grs_sls_extnd_ytd) / SUM(tmp.grs_sls_extnd_ytd)) * 100
                                     ELSE
                                        0
                                  END
                                     ir_pct_ytd,
                                  SUM(tmp.grs_sls_extnd) grs_sls_extnd,
                                  SUM(tmp.grs_sls_extnd_ly) grs_sls_extnd_ly,
                                  CASE
                                     WHEN SUM(tmp.grs_sls_extnd_ly) > 0 THEN
                                        ((SUM(tmp.grs_sls_extnd) - SUM(tmp.grs_sls_extnd_ly)) / SUM(tmp.grs_sls_extnd_ly))
                                        * 100
                                     ELSE
                                        0
                                  END
                                     grs_sls_extnd_var,
                                  SUM(tmp.grs_sls_extnd_ytd) grs_sls_extnd_ytd,
                                  SUM(tmp.grs_sls_extnd_ytd_ly) grs_sls_extnd_ytd_ly,
                                  CASE
                                     WHEN SUM(tmp.grs_sls_extnd_ytd_ly) > 0 THEN
                                        ((SUM(tmp.grs_sls_extnd_ytd) - SUM(tmp.grs_sls_extnd_ytd_ly))
                                         / SUM(tmp.grs_sls_extnd_ytd_ly))
                                        * 100
                                     ELSE
                                        0
                                  END
                                     grs_sls_extnd_ytd_var,
                                  SUM(tmp.unfrm_qty_ship) unfrm_qty_ship,
                                  SUM(tmp.unfrm_qty_ship_ly) unfrm_qty_ship_ly,
                                  CASE
                                     WHEN SUM(tmp.unfrm_qty_ship_ly) > 0 THEN
                                        ((SUM(tmp.unfrm_qty_ship) - SUM(tmp.unfrm_qty_ship_ly)) / SUM(tmp.unfrm_qty_ship_ly))
                                        * 100
                                     ELSE
                                        0
                                  END
                                     unfrm_qty_ship_var,
                                  SUM(tmp.unfrm_qty_ship_ytd) unfrm_qty_ship_ytd,
                                  SUM(tmp.unfrm_qty_ship_ytd_ly) unfrm_qty_ship_ytd_ly,
                                  CASE
                                     WHEN SUM(tmp.unfrm_qty_ship_ytd_ly) > 0 THEN
                                        ((SUM(tmp.unfrm_qty_ship_ytd) - SUM(tmp.unfrm_qty_ship_ytd_ly))
                                         / SUM(tmp.unfrm_qty_ship_ytd_ly))
                                        * 100
                                     ELSE
                                        0
                                  END
                                     unfrm_qty_ship_ytd_var,
                                  SUM(tmp.grs_tgp) grs_tgp,
                                  SUM(tmp.grs_tgp_ly) grs_tgp_ly,
                                  CASE
                                     WHEN SUM(tmp.grs_tgp_ly) > 0 THEN
                                        ((SUM(tmp.grs_tgp) - SUM(tmp.grs_tgp_ly)) / SUM(tmp.grs_tgp_ly)) * 100
                                     ELSE
                                        0
                                  END
                                     grs_tgp_var,
                                  SUM(tmp.grs_tgp_ytd) grs_tgp_ytd,
                                  SUM(tmp.grs_tgp_ytd_ly) grs_tgp_ytd_ly,
                                  CASE
                                     WHEN SUM(tmp.grs_tgp_ytd_ly) > 0 THEN
                                        ((SUM(tmp.grs_tgp_ytd) - SUM(tmp.grs_tgp_ytd_ly)) / SUM(tmp.grs_tgp_ytd_ly)) * 100
                                     ELSE
                                        0
                                  END
                                     grs_tgp_ytd_var,
                                  SUM(tmp.net_acct) net_acct,
                                  SUM(tmp.net_acct_ly) net_acct_ly,
                                  CASE
                                     WHEN SUM(tmp.net_acct_ly) > 0 THEN
                                        ((SUM(tmp.net_acct) - SUM(tmp.net_acct_ly)) / SUM(tmp.net_acct_ly)) * 100
                                     ELSE
                                        0
                                  END
                                     net_acct_var,
                                  CASE WHEN org.tm_cnt > 0 THEN SUM(tmp.net_acct) / org.tm_cnt ELSE 0 END net_acct_per_tm,
                                  CASE WHEN org.tm_cnt_ly > 0 THEN SUM(tmp.net_acct_ly) / org.tm_cnt_ly ELSE 0 END
                                     net_acct_per_tm_ly,
                                  CASE
                                     WHEN CASE WHEN org.tm_cnt_ly > 0 THEN SUM(tmp.net_acct_ly) / org.tm_cnt_ly ELSE 0 END > 0 THEN
                                        (CASE WHEN org.tm_cnt > 0 THEN SUM(tmp.net_acct) / org.tm_cnt ELSE 0 END
                                         - CASE WHEN org.tm_cnt_ly > 0 THEN SUM(tmp.net_acct_ly) / org.tm_cnt_ly ELSE 0 END)
                                        / CASE WHEN org.tm_cnt_ly > 0 THEN SUM(tmp.net_acct_ly) / org.tm_cnt_ly ELSE 0 END
                                        * 100
                                     ELSE
                                        0
                                  END
                                     net_acct_per_tm_var,
                                  CASE
                                     WHEN SUM(tmp.drop_cnt) > 0 THEN
                                        SUM(tmp.rt_ln_cnt + tmp.vndr_ship_ln_cnt + tmp.wc_ln_cnt) / SUM(tmp.drop_cnt)
                                     ELSE
                                        0
                                  END
                                     lines_per_drop,
                                  CASE
                                     WHEN SUM(tmp.drop_cnt_ly) > 0 THEN
                                        SUM(tmp.rt_ln_cnt_ly + tmp.vndr_ship_ln_cnt_ly + tmp.wc_ln_cnt_ly)
                                        / SUM(tmp.drop_cnt_ly)
                                     ELSE
                                        0
                                  END
                                     lines_per_drop_ly,
                                  CASE
                                     WHEN CASE
                                             WHEN SUM(tmp.drop_cnt_ly) > 0 THEN
                                                SUM(tmp.rt_ln_cnt_ly + tmp.vndr_ship_ln_cnt_ly + tmp.wc_ln_cnt_ly)
                                                / SUM(tmp.drop_cnt_ly)
                                             ELSE
                                                0
                                          END > 0 THEN
                                        (CASE
                                            WHEN SUM(tmp.drop_cnt) > 0 THEN
                                               SUM(tmp.rt_ln_cnt + tmp.vndr_ship_ln_cnt + tmp.wc_ln_cnt) / SUM(tmp.drop_cnt)
                                            ELSE
                                               0
                                         END
                                         - CASE
                                              WHEN SUM(tmp.drop_cnt_ly) > 0 THEN
                                                 SUM(tmp.rt_ln_cnt_ly + tmp.vndr_ship_ln_cnt_ly + tmp.wc_ln_cnt_ly)
                                                 / SUM(tmp.drop_cnt_ly)
                                              ELSE
                                                 0
                                           END)
                                        / CASE
                                             WHEN SUM(tmp.drop_cnt_ly) > 0 THEN
                                                SUM(tmp.rt_ln_cnt_ly + tmp.vndr_ship_ln_cnt_ly + tmp.wc_ln_cnt_ly)
                                                / SUM(tmp.drop_cnt_ly)
                                             ELSE
                                                0
                                          END
                                        * 100
                                     ELSE
                                        0
                                  END
                                     lines_per_drop_var,
                                  CASE
                                     WHEN SUM(tmp.grs_sls_extnd) > 0 THEN
                                        (SUM(tmp.eb_grs_sls_extnd) / SUM(tmp.grs_sls_extnd)) * 100
                                     ELSE
                                        0
                                  END
                                     eb_pct,
                                  CASE
                                     WHEN SUM(tmp.grs_sls_extnd_ly) > 0 THEN
                                        (SUM(tmp.eb_grs_sls_extnd_ly) / SUM(tmp.grs_sls_extnd_ly)) * 100
                                     ELSE
                                        0
                                  END
                                     eb_pct_ly
                             FROM (SELECT *
                                     FROM xdmadm.sls_rnk_tmp
                                    WHERE '
                  || r_rnk_ctgry.rnk_predicate
                  || '
                                      AND '
                  || r_cust_clsfctn.predicate
                  || ') tmp,
                            (SELECT o.*, CASE WHEN '
                  || r_rnk_ctgry.rnk_ctgry
                  || ' <= 3 THEN tm_st_cnt ELSE tm_na_cnt END tm_cnt,
                                         CASE WHEN '
                  || r_rnk_ctgry.rnk_ctgry
                  || ' <= 3 THEN tm_st_cnt_ly ELSE tm_na_cnt_ly END tm_cnt_ly
                               FROM xdmadm.sls_rnk_org_tmp o) org
                            WHERE '
                  || r_org_lvl.predicate
                  || '
                              AND org.org_lvl = '''
                  || r_org_lvl.org_lvl
                  || '''
                              AND org.fisc_yr_mth = '''
                  || g_fisc_yr_mth
                  || '''
                         GROUP BY org.org_val, org.tm_cnt, org.tm_cnt_ly)
                 SELECT '''
                  || g_fisc_yr_mth
                  || ''' fisc_yr_mth,
                        '''
                  || r_org_lvl.org_lvl
                  || ''' org_lvl,
                        pop.org_val, '
                  || r_rnk_ctgry.rnk_ctgry
                  || ' rnk_ctgry,
                        pv.param_code rnk_lvl,
                        '''
                  || r_cust_clsfctn.cust_clsfctn
                  || ''' cust_clsfctn,
                        ''VAR PERCENT'' rnk_by,
                        pop.grp_cd_or_brnch_cd,
                        rnk.ec_pct,
                        rnk.ir_pct,
                        rnk.ir_pct_ytd,
                        rnk.grs_sls_extnd,
                        rnk.grs_sls_extnd_ly,
                        CASE WHEN rnk.grs_sls_extnd_var > 99999999999.99 THEN 99999999999.99 ELSE rnk.grs_sls_extnd_var END grs_sls_extnd_var,
                        rnk.grs_sls_extnd_ytd,
                        rnk.grs_sls_extnd_ytd_ly,
                        CASE WHEN rnk.grs_sls_extnd_ytd_var > 99999999999.99 THEN 99999999999.99 ELSE rnk.grs_sls_extnd_ytd_var END grs_sls_extnd_ytd_var,
                        rnk.unfrm_qty_ship,
                        rnk.unfrm_qty_ship_ly,
                        CASE WHEN rnk.unfrm_qty_ship_var > 999999999.9999 THEN 999999999.9999 ELSE rnk.unfrm_qty_ship_var END unfrm_qty_ship_var,
                        rnk.unfrm_qty_ship_ytd,
                        rnk.unfrm_qty_ship_ytd_ly,
                        CASE WHEN rnk.unfrm_qty_ship_ytd_var > 999999999.9999 THEN 999999999.9999 ELSE rnk.unfrm_qty_ship_ytd_var END unfrm_qty_ship_ytd_var,
                        rnk.grs_tgp,
                        rnk.grs_tgp_ly,
                        CASE WHEN rnk.grs_tgp_var > 999999999.99999999 THEN 999999999.99999999 ELSE rnk.grs_tgp_var END grs_tgp_var,
                        rnk.grs_tgp_ytd,
                        rnk.grs_tgp_ytd_ly,
                        CASE WHEN rnk.grs_tgp_ytd_var > 999999999.99999999 THEN 999999999.99999999 ELSE rnk.grs_tgp_ytd_var END grs_tgp_ytd_var,
                        rnk.net_acct,
                        rnk.net_acct_ly,
                        CASE WHEN rnk.net_acct_var > 99999999.99 THEN 99999999.99 ELSE rnk.net_acct_var END net_acct_var,
                        rnk.net_acct_per_tm,
                        rnk.net_acct_per_tm_ly,
                        CASE WHEN rnk.net_acct_per_tm_var > 9999999.9999 THEN 9999999.9999 ELSE rnk.net_acct_per_tm_var END net_acct_per_tm_var,
                        rnk.lines_per_drop,
                        rnk.lines_per_drop_ly,
                        CASE WHEN rnk.lines_per_drop_var > 9999999.9999 THEN 9999999.9999 ELSE rnk.lines_per_drop_var END lines_per_drop_var,
                        rnk.eb_pct,
                        rnk.eb_pct_ly,
                        NULL
                   FROM pop, rnk, xdmadm.param_value pv
                  WHERE pop.org_val = rnk.org_val(+)
                    AND pv.app_name = ''SLS_RNK''
                    AND pv.param_type = ''RNK_LVL_LOV'')';

               EXECUTE IMMEDIATE l_sql;

               COMMIT;
            END LOOP;
         END LOOP;
      END LOOP;

      -- Get stats on SLS_RNK_MTH_TMP table
      l_return_code   := xdmadm.fn_getstat_tbl('XDMADM', 'SLS_RNK_MTH_TMP');
      DBMS_OUTPUT.put_line('Analyze SLS_RNK_MTH_TMP Return Code: ' || l_return_code);

      -- Update SLS_RNK_MTH_TMP to set the IR percent values from ranking category 0 to other ranking categories
      UPDATE xdmadm.sls_rnk_mth_tmp s
         SET ir_pct   =
                (SELECT ir_pct
                   FROM xdmadm.sls_rnk_mth_tmp
                  WHERE org_lvl = s.org_lvl
                    AND org_val = s.org_val
                    AND cust_clsfctn = s.cust_clsfctn
                    AND rnk_by = s.rnk_by
                    AND rnk_lvl = s.rnk_lvl
                    AND rnk_ctgry = 0),
             ir_pct_ytd   =
                (SELECT ir_pct_ytd
                   FROM xdmadm.sls_rnk_mth_tmp
                  WHERE org_lvl = s.org_lvl
                    AND org_val = s.org_val
                    AND cust_clsfctn = s.cust_clsfctn
                    AND rnk_by = s.rnk_by
                    AND rnk_lvl = s.rnk_lvl
                    AND rnk_ctgry = 0)
       WHERE rnk_ctgry <> 0;

      IF l_return_code <> 0 THEN
         DBMS_OUTPUT.put_line('Error: Failed to analyze table SLS_RNK_MTH_TMP');
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line(l_sql);
         RAISE;
   END ins_sls_rnk_mth;

   /***********************************************************************
   * Name: UPD_SLS_RNK_MTH_CNT
   * Type: Procedure
   * Description: Updates SLS_RNK_MTH_TMP table with period and YTD population counts
                  based on eligibility flags in SLS_RNK_ORG_TMP table
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        06/05/2013  Matt Nicol       Initial Creation
   ************************************************************************/

   PROCEDURE upd_sls_rnk_mth_cnt IS
      -- Cursor to get population counts for org entities ranked by division
      CURSOR c_div_cnt IS
           SELECT rnk.fisc_yr_mth,
                  rnk.org_lvl,
                  rnk.rnk_ctgry,
                  rnk.cust_clsfctn,
                  rnk.grp_cd_or_brnch_cd,
                  SUM(CASE
                         WHEN ((rnk.rnk_ctgry <= 3
                            AND org.mth_st_elgbl = 'Y')
                            OR (rnk.rnk_ctgry >= 4
                            AND org.mth_na_elgbl = 'Y')) THEN
                            1
                         ELSE
                            0
                      END)
                     mth_pop,
                  SUM(CASE
                         WHEN ((rnk.rnk_ctgry <= 3
                            AND org.ytd_st_elgbl = 'Y')
                            OR (rnk.rnk_ctgry >= 4
                            AND org.ytd_na_elgbl = 'Y')) THEN
                            1
                         ELSE
                            0
                      END)
                     ytd_pop
             FROM xdmadm.sls_rnk_mth_tmp rnk, xdmadm.sls_rnk_org_tmp org
            WHERE rnk.fisc_yr_mth = org.fisc_yr_mth
              AND rnk.org_lvl = org.org_lvl
              AND rnk.org_val = org.org_val
              AND rnk.rnk_lvl = 'DIVISION'
              AND rnk.rnk_by = 'VAR'
              AND org.fisc_yr_mth = g_fisc_yr_mth
         GROUP BY rnk.fisc_yr_mth,
                  rnk.org_lvl,
                  rnk.rnk_ctgry,
                  rnk.cust_clsfctn,
                  rnk.grp_cd_or_brnch_cd;

      TYPE t_div_cnt IS TABLE OF c_div_cnt%ROWTYPE;

      r_div_cnt   t_div_cnt;

      -- Cursor to get population counts for org entities ranked nationally
      CURSOR c_nat_cnt IS
           SELECT rnk.fisc_yr_mth,
                  rnk.org_lvl,
                  rnk.rnk_ctgry,
                  rnk.cust_clsfctn,
                  SUM(CASE
                         WHEN ((rnk.rnk_ctgry <= 3
                            AND org.mth_st_elgbl = 'Y')
                            OR (rnk.rnk_ctgry >= 4
                            AND org.mth_na_elgbl = 'Y')) THEN
                            1
                         ELSE
                            0
                      END)
                     mth_pop,
                  SUM(CASE
                         WHEN ((rnk.rnk_ctgry <= 3
                            AND org.ytd_st_elgbl = 'Y')
                            OR (rnk.rnk_ctgry >= 4
                            AND org.ytd_na_elgbl = 'Y')) THEN
                            1
                         ELSE
                            0
                      END)
                     ytd_pop
             FROM xdmadm.sls_rnk_mth_tmp rnk, xdmadm.sls_rnk_org_tmp org
            WHERE rnk.fisc_yr_mth = org.fisc_yr_mth
              AND rnk.org_lvl = org.org_lvl
              AND rnk.org_val = org.org_val
              AND rnk.rnk_lvl = 'NATIONAL'
              AND rnk.rnk_by = 'VAR'
              AND org.fisc_yr_mth = g_fisc_yr_mth
         GROUP BY rnk.fisc_yr_mth,
                  rnk.org_lvl,
                  rnk.rnk_ctgry,
                  rnk.cust_clsfctn;

      TYPE t_nat_cnt IS TABLE OF c_nat_cnt%ROWTYPE;

      r_nat_cnt   t_nat_cnt;
   BEGIN
      DBMS_OUTPUT.put_line('Updating counts in SLS_RNK_MTH_TMP');

      -- Update population counts in SLS_RNK_MTH_TMP for org entities ranked by division
      OPEN c_div_cnt;

      LOOP
         EXIT WHEN c_div_cnt%NOTFOUND;

         FETCH c_div_cnt
         BULK COLLECT INTO r_div_cnt
         LIMIT 50000;

         FORALL i IN 1 .. r_div_cnt.COUNT()
            UPDATE xdmadm.sls_rnk_mth_tmp
               SET mth_pop = r_div_cnt(i).mth_pop, ytd_pop = r_div_cnt(i).ytd_pop
             WHERE fisc_yr_mth = r_div_cnt(i).fisc_yr_mth
               AND org_lvl = r_div_cnt(i).org_lvl
               AND rnk_ctgry = r_div_cnt(i).rnk_ctgry
               AND cust_clsfctn = r_div_cnt(i).cust_clsfctn
               AND grp_cd_or_brnch_cd = r_div_cnt(i).grp_cd_or_brnch_cd
               AND rnk_lvl = 'DIVISION';
      END LOOP;

      CLOSE c_div_cnt;

      COMMIT;

      -- Update population counts in SLS_RNK_MTH_TMP for org entities ranked nationally
      OPEN c_nat_cnt;

      LOOP
         EXIT WHEN c_nat_cnt%NOTFOUND;

         FETCH c_nat_cnt
         BULK COLLECT INTO r_nat_cnt
         LIMIT 50000;

         FORALL i IN 1 .. r_nat_cnt.COUNT()
            UPDATE xdmadm.sls_rnk_mth_tmp
               SET mth_pop = r_nat_cnt(i).mth_pop, ytd_pop = r_nat_cnt(i).ytd_pop
             WHERE fisc_yr_mth = r_div_cnt(i).fisc_yr_mth
               AND org_lvl = r_nat_cnt(i).org_lvl
               AND rnk_ctgry = r_nat_cnt(i).rnk_ctgry
               AND cust_clsfctn = r_nat_cnt(i).cust_clsfctn
               AND rnk_lvl = 'NATIONAL';
      END LOOP;

      CLOSE c_nat_cnt;

      COMMIT;
   END upd_sls_rnk_mth_cnt;

   /***********************************************************************
   * Name: UPD_SLS_RNK_MTH_RNK
   * Type: Procedure
   * Description: Updates SLS_RNK_MTH table with rank and percentile data
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        06/05/2013  Matt Nicol       Initial Creation
   ************************************************************************/

   PROCEDURE upd_sls_rnk_mth_rnk IS
      --Cursor to get month rank values for org entities ranked by division
      CURSOR c_mth_div_rnk IS
         SELECT fisc_yr_mth,
                org_lvl,
                org_val,
                rnk_ctgry,
                cust_clsfctn,
                rnk_by,
                ec_pct_rnk,
                ec_pct_rnk_pctl,
                ir_pct_rnk,
                ir_pct_rnk_pctl,
                grs_sls_extnd_rnk,
                grs_sls_extnd_rnk_pctl,
                unfrm_qty_ship_rnk,
                unfrm_qty_ship_rnk_pctl,
                grs_tgp_rnk,
                grs_tgp_rnk_pctl,
                net_acct_rnk,
                net_acct_rnk_pctl,
                net_acct_per_tm_rnk,
                net_acct_per_tm_rnk_pctl,
                lines_per_drop_rnk,
                lines_per_drop_rnk_pctl,
                eb_pct_rnk,
                eb_pct_rnk_pctl,
                RANK()
                   OVER (PARTITION BY fisc_yr_mth, org_lvl, rnk_ctgry, cust_clsfctn, rnk_by, grp_cd_or_brnch_cd
                         ORDER BY ir_pct_rnk + unfrm_qty_ship_rnk + grs_tgp_rnk)
                   overall_rnk,
                PERCENT_RANK()
                   OVER (PARTITION BY fisc_yr_mth, org_lvl, rnk_ctgry, cust_clsfctn, rnk_by, grp_cd_or_brnch_cd
                         ORDER BY ir_pct_rnk + unfrm_qty_ship_rnk + grs_tgp_rnk DESC)
                   overall_rnk_pctl
           FROM (SELECT rnk.fisc_yr_mth,
                        rnk.org_lvl,
                        rnk.org_val,
                        rnk.rnk_ctgry,
                        rnk.cust_clsfctn,
                        rnk.rnk_by,
                        org.grp_cd_or_brnch_cd,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.ec_pct DESC NULLS LAST)
                           ec_pct_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.ec_pct NULLS FIRST)
                           ec_pct_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.ir_pct DESC NULLS LAST)
                           ir_pct_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.ir_pct NULLS FIRST)
                           ir_pct_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.grs_sls_extnd_var DESC NULLS LAST)
                           grs_sls_extnd_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.grs_sls_extnd_var NULLS FIRST)
                           grs_sls_extnd_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.unfrm_qty_ship_var DESC NULLS LAST)
                           unfrm_qty_ship_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.unfrm_qty_ship_var NULLS FIRST)
                           unfrm_qty_ship_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.grs_tgp_var DESC NULLS LAST)
                           grs_tgp_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.grs_tgp_var NULLS FIRST)
                           grs_tgp_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.net_acct_var DESC NULLS LAST)
                           net_acct_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.net_acct_var NULLS FIRST)
                           net_acct_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.net_acct_per_tm_var DESC NULLS LAST)
                           net_acct_per_tm_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.net_acct_per_tm_var NULLS FIRST)
                           net_acct_per_tm_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.lines_per_drop_var DESC NULLS LAST)
                           lines_per_drop_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.lines_per_drop_var NULLS FIRST)
                           lines_per_drop_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.eb_pct_var DESC NULLS LAST)
                           eb_pct_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.eb_pct_var NULLS FIRST)
                           eb_pct_rnk_pctl
                   FROM xdmadm.sls_rnk_mth_tmp rnk, xdmadm.sls_rnk_org_tmp org
                  WHERE rnk.fisc_yr_mth = org.fisc_yr_mth
                    AND rnk.org_lvl = org.org_lvl
                    AND rnk.org_val = org.org_val
                    AND ((rnk.rnk_ctgry <= 3
                      AND org.mth_st_elgbl = 'Y')
                      OR (rnk.rnk_ctgry >= 4
                      AND org.mth_na_elgbl = 'Y'))
                    AND rnk.rnk_lvl = 'DIVISION'
                    AND org.fisc_yr_mth = g_fisc_yr_mth);

      TYPE t_mth_div_rnk IS TABLE OF c_mth_div_rnk%ROWTYPE;

      r_mth_div_rnk   t_mth_div_rnk;

      --Cursor to get month rank values for org entities ranked nationally
      CURSOR c_mth_nat_rnk IS
         SELECT fisc_yr_mth,
                org_lvl,
                org_val,
                rnk_ctgry,
                cust_clsfctn,
                rnk_by,
                ec_pct_rnk,
                ec_pct_rnk_pctl,
                ir_pct_rnk,
                ir_pct_rnk_pctl,
                grs_sls_extnd_rnk,
                grs_sls_extnd_rnk_pctl,
                unfrm_qty_ship_rnk,
                unfrm_qty_ship_rnk_pctl,
                grs_tgp_rnk,
                grs_tgp_rnk_pctl,
                net_acct_rnk,
                net_acct_rnk_pctl,
                net_acct_per_tm_rnk,
                net_acct_per_tm_rnk_pctl,
                lines_per_drop_rnk,
                lines_per_drop_rnk_pctl,
                eb_pct_rnk,
                eb_pct_rnk_pctl,
                RANK()
                   OVER (PARTITION BY fisc_yr_mth, org_lvl, rnk_ctgry, cust_clsfctn, rnk_by ORDER BY ir_pct_rnk + unfrm_qty_ship_rnk + grs_tgp_rnk)
                   overall_rnk,
                PERCENT_RANK()
                   OVER (PARTITION BY fisc_yr_mth, org_lvl, rnk_ctgry, cust_clsfctn, rnk_by
                         ORDER BY ir_pct_rnk + unfrm_qty_ship_rnk + grs_tgp_rnk DESC)
                   overall_rnk_pctl
           FROM (SELECT rnk.fisc_yr_mth,
                        rnk.org_lvl,
                        rnk.org_val,
                        rnk.rnk_ctgry,
                        rnk.cust_clsfctn,
                        rnk.rnk_by,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.ec_pct DESC NULLS LAST)
                           ec_pct_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.ec_pct NULLS FIRST)
                           ec_pct_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.ir_pct DESC NULLS LAST)
                           ir_pct_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.ir_pct NULLS FIRST)
                           ir_pct_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.grs_sls_extnd_var DESC NULLS LAST)
                           grs_sls_extnd_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.grs_sls_extnd_var NULLS FIRST)
                           grs_sls_extnd_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.unfrm_qty_ship_var DESC NULLS LAST)
                           unfrm_qty_ship_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.unfrm_qty_ship_var NULLS FIRST)
                           unfrm_qty_ship_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.grs_tgp_var DESC NULLS LAST)
                           grs_tgp_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.grs_tgp_var NULLS FIRST)
                           grs_tgp_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.net_acct_var DESC NULLS LAST)
                           net_acct_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.net_acct_var NULLS FIRST)
                           net_acct_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.net_acct_per_tm_var DESC NULLS LAST)
                           net_acct_per_tm_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.net_acct_per_tm_var NULLS FIRST)
                           net_acct_per_tm_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.lines_per_drop_var DESC NULLS LAST)
                           lines_per_drop_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.lines_per_drop_var NULLS FIRST)
                           lines_per_drop_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.eb_pct_var DESC NULLS LAST)
                           eb_pct_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.eb_pct_var NULLS FIRST)
                           eb_pct_rnk_pctl
                   FROM xdmadm.sls_rnk_mth_tmp rnk, xdmadm.sls_rnk_org_tmp org
                  WHERE rnk.fisc_yr_mth = org.fisc_yr_mth
                    AND rnk.org_lvl = org.org_lvl
                    AND rnk.org_val = org.org_val
                    AND ((rnk.rnk_ctgry <= 3
                      AND org.mth_st_elgbl = 'Y')
                      OR (rnk.rnk_ctgry >= 4
                      AND org.mth_na_elgbl = 'Y'))
                    AND rnk.rnk_lvl = 'NATIONAL'
                    AND org.fisc_yr_mth = g_fisc_yr_mth);

      TYPE t_mth_nat_rnk IS TABLE OF c_mth_nat_rnk%ROWTYPE;

      r_mth_nat_rnk   t_mth_nat_rnk;

      --Cursor to get year to date rank values for org entities ranked by division
      CURSOR c_ytd_div_rnk IS
         SELECT fisc_yr_mth,
                org_lvl,
                org_val,
                rnk_ctgry,
                cust_clsfctn,
                rnk_by,
                ir_pct_ytd_rnk,
                grs_sls_extnd_ytd_rnk,
                unfrm_qty_ship_ytd_rnk,
                grs_tgp_ytd_rnk,
                ir_pct_ytd_rnk_pctl,
                grs_sls_extnd_ytd_rnk_pctl,
                unfrm_qty_ship_ytd_rnk_pctl,
                grs_tgp_ytd_rnk_pctl,
                RANK()
                   OVER (PARTITION BY fisc_yr_mth, org_lvl, rnk_ctgry, cust_clsfctn, rnk_by, grp_cd_or_brnch_cd
                         ORDER BY unfrm_qty_ship_ytd_rnk + grs_tgp_ytd_rnk + ir_pct_ytd_rnk)
                   overall_ytd_rnk,
                PERCENT_RANK()
                   OVER (PARTITION BY fisc_yr_mth, org_lvl, rnk_ctgry, cust_clsfctn, rnk_by, grp_cd_or_brnch_cd
                         ORDER BY unfrm_qty_ship_ytd_rnk + grs_tgp_ytd_rnk + ir_pct_ytd_rnk DESC)
                   overall_ytd_rnk_pctl
           FROM (SELECT rnk.fisc_yr_mth,
                        rnk.org_lvl,
                        rnk.org_val,
                        rnk.rnk_ctgry,
                        rnk.cust_clsfctn,
                        rnk.rnk_by,
                        org.grp_cd_or_brnch_cd,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.ir_pct_ytd DESC NULLS LAST)
                           ir_pct_ytd_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.ir_pct_ytd NULLS FIRST)
                           ir_pct_ytd_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.grs_sls_extnd_ytd_var DESC NULLS LAST)
                           grs_sls_extnd_ytd_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.grs_sls_extnd_ytd_var NULLS FIRST)
                           grs_sls_extnd_ytd_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.unfrm_qty_ship_ytd_var DESC NULLS LAST)
                           unfrm_qty_ship_ytd_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.unfrm_qty_ship_ytd_var NULLS FIRST)
                           unfrm_qty_ship_ytd_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.grs_tgp_ytd_var DESC NULLS LAST)
                           grs_tgp_ytd_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by, org.grp_cd_or_brnch_cd
                                 ORDER BY rnk.grs_tgp_ytd_var NULLS FIRST)
                           grs_tgp_ytd_rnk_pctl
                   FROM xdmadm.sls_rnk_mth_tmp rnk, xdmadm.sls_rnk_org_tmp org
                  WHERE rnk.fisc_yr_mth = org.fisc_yr_mth
                    AND rnk.org_lvl = org.org_lvl
                    AND rnk.org_val = org.org_val
                    AND ((rnk.rnk_ctgry <= 3
                      AND org.ytd_st_elgbl = 'Y')
                      OR (rnk.rnk_ctgry >= 4
                      AND org.ytd_na_elgbl = 'Y'))
                    AND rnk.rnk_lvl = 'DIVISION'
                    AND org.fisc_yr_mth = g_fisc_yr_mth);

      TYPE t_ytd_div_rnk IS TABLE OF c_ytd_div_rnk%ROWTYPE;

      r_ytd_div_rnk   t_ytd_div_rnk;

      --Cursor to get year to date rank values for org entities ranked nationally
      CURSOR c_ytd_nat_rnk IS
         SELECT fisc_yr_mth,
                org_lvl,
                org_val,
                rnk_ctgry,
                cust_clsfctn,
                rnk_by,
                ir_pct_ytd_rnk,
                ir_pct_ytd_rnk_pctl,
                grs_sls_extnd_ytd_rnk,
                grs_sls_extnd_ytd_rnk_pctl,
                unfrm_qty_ship_ytd_rnk,
                unfrm_qty_ship_ytd_rnk_pctl,
                grs_tgp_ytd_rnk,
                grs_tgp_ytd_rnk_pctl,
                RANK()
                   OVER (PARTITION BY fisc_yr_mth, org_lvl, rnk_ctgry, cust_clsfctn, rnk_by
                         ORDER BY unfrm_qty_ship_ytd_rnk + grs_tgp_ytd_rnk + ir_pct_ytd_rnk)
                   overall_ytd_rnk,
                PERCENT_RANK()
                   OVER (PARTITION BY fisc_yr_mth, org_lvl, rnk_ctgry, cust_clsfctn, rnk_by
                         ORDER BY unfrm_qty_ship_ytd_rnk + grs_tgp_ytd_rnk + ir_pct_ytd_rnk DESC)
                   overall_ytd_rnk_pctl
           FROM (SELECT rnk.fisc_yr_mth,
                        rnk.org_lvl,
                        rnk.org_val,
                        rnk.rnk_ctgry,
                        rnk.cust_clsfctn,
                        rnk.rnk_by,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.ir_pct_ytd DESC NULLS LAST)
                           ir_pct_ytd_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.ir_pct_ytd NULLS FIRST)
                           ir_pct_ytd_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.grs_sls_extnd_ytd_var DESC NULLS LAST)
                           grs_sls_extnd_ytd_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.grs_sls_extnd_ytd_var NULLS FIRST)
                           grs_sls_extnd_ytd_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.unfrm_qty_ship_ytd_var DESC NULLS LAST)
                           unfrm_qty_ship_ytd_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.unfrm_qty_ship_ytd_var NULLS FIRST)
                           unfrm_qty_ship_ytd_rnk_pctl,
                        RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.grs_tgp_ytd_var DESC NULLS LAST)
                           grs_tgp_ytd_rnk,
                        PERCENT_RANK()
                           OVER (PARTITION BY rnk.fisc_yr_mth, rnk.org_lvl, rnk.rnk_ctgry, rnk.cust_clsfctn, rnk.rnk_by
                                 ORDER BY rnk.grs_tgp_ytd_var NULLS FIRST)
                           grs_tgp_ytd_rnk_pctl
                   FROM xdmadm.sls_rnk_mth_tmp rnk, xdmadm.sls_rnk_org_tmp org
                  WHERE rnk.fisc_yr_mth = org.fisc_yr_mth
                    AND rnk.org_lvl = org.org_lvl
                    AND rnk.org_val = org.org_val
                    AND ((rnk.rnk_ctgry <= 3
                      AND org.ytd_st_elgbl = 'Y')
                      OR (rnk.rnk_ctgry >= 4
                      AND org.ytd_na_elgbl = 'Y'))
                    AND rnk.rnk_lvl = 'NATIONAL'
                    AND org.fisc_yr_mth = g_fisc_yr_mth);

      TYPE t_ytd_nat_rnk IS TABLE OF c_ytd_nat_rnk%ROWTYPE;

      r_ytd_nat_rnk   t_ytd_nat_rnk;
   BEGIN
      DBMS_OUTPUT.put_line('Updating ranks in SLS_RNK_MTH_TMP');

      --Update month rank values in SLS_RNK_MTH_TMP for org entities ranked by division
      OPEN c_mth_div_rnk;

      LOOP
         EXIT WHEN c_mth_div_rnk%NOTFOUND;

         FETCH c_mth_div_rnk
         BULK COLLECT INTO r_mth_div_rnk
         LIMIT 50000;

         FORALL i IN 1 .. r_mth_div_rnk.COUNT()
            UPDATE xdmadm.sls_rnk_mth_tmp
               SET overall_rnk                = r_mth_div_rnk(i).overall_rnk,
                   overall_rnk_pctl           = r_mth_div_rnk(i).overall_rnk_pctl * 100,
                   ec_pct_rnk                 = r_mth_div_rnk(i).ec_pct_rnk,
                   ec_pct_rnk_pctl            = r_mth_div_rnk(i).ec_pct_rnk_pctl * 100,
                   ir_pct_rnk                 = r_mth_div_rnk(i).ir_pct_rnk,
                   ir_pct_pctl                = r_mth_div_rnk(i).ir_pct_rnk_pctl * 100,
                   grs_sls_extnd_rnk          = r_mth_div_rnk(i).grs_sls_extnd_rnk,
                   grs_sls_extnd_rnk_pctl     = r_mth_div_rnk(i).grs_sls_extnd_rnk_pctl * 100,
                   unfrm_qty_ship_rnk         = r_mth_div_rnk(i).unfrm_qty_ship_rnk,
                   unfrm_qty_ship_rnk_pctl    = r_mth_div_rnk(i).unfrm_qty_ship_rnk_pctl * 100,
                   grs_tgp_rnk                = r_mth_div_rnk(i).grs_tgp_rnk,
                   grs_tgp_rnk_pctl           = r_mth_div_rnk(i).grs_tgp_rnk_pctl * 100,
                   net_acct_rnk               = r_mth_div_rnk(i).net_acct_rnk,
                   net_acct_rnk_pctl          = r_mth_div_rnk(i).net_acct_rnk_pctl * 100,
                   net_acct_per_tm_rnk        = r_mth_div_rnk(i).net_acct_per_tm_rnk,
                   net_acct_per_tm_rnk_pctl   = r_mth_div_rnk(i).net_acct_per_tm_rnk_pctl * 100,
                   lines_per_drop_rnk         = r_mth_div_rnk(i).lines_per_drop_rnk,
                   lines_per_drop_rnk_pctl    = r_mth_div_rnk(i).lines_per_drop_rnk_pctl * 100,
                   eb_pct_rnk                 = r_mth_div_rnk(i).eb_pct_rnk,
                   eb_pct_rnk_pctl            = r_mth_div_rnk(i).eb_pct_rnk_pctl * 100
             WHERE fisc_yr_mth = r_mth_div_rnk(i).fisc_yr_mth
               AND org_lvl = r_mth_div_rnk(i).org_lvl
               AND org_val = r_mth_div_rnk(i).org_val
               AND rnk_ctgry = r_mth_div_rnk(i).rnk_ctgry
               AND cust_clsfctn = r_mth_div_rnk(i).cust_clsfctn
               AND rnk_by = r_mth_div_rnk(i).rnk_by
               AND rnk_lvl = 'DIVISION';
      END LOOP;

      CLOSE c_mth_div_rnk;

      COMMIT;

      --Update month rank values in SLS_RNK_MTH_TMP for org entities ranked nationally
      OPEN c_mth_nat_rnk;

      LOOP
         EXIT WHEN c_mth_nat_rnk%NOTFOUND;

         FETCH c_mth_nat_rnk
         BULK COLLECT INTO r_mth_nat_rnk
         LIMIT 50000;

         FORALL i IN 1 .. r_mth_nat_rnk.COUNT()
            UPDATE xdmadm.sls_rnk_mth_tmp
               SET overall_rnk                = r_mth_nat_rnk(i).overall_rnk,
                   overall_rnk_pctl           = r_mth_nat_rnk(i).overall_rnk_pctl * 100,
                   ec_pct_rnk                 = r_mth_nat_rnk(i).ec_pct_rnk,
                   ec_pct_rnk_pctl            = r_mth_nat_rnk(i).ec_pct_rnk_pctl * 100,
                   ir_pct_rnk                 = r_mth_nat_rnk(i).ir_pct_rnk,
                   ir_pct_pctl                = r_mth_nat_rnk(i).ir_pct_rnk_pctl * 100,
                   grs_sls_extnd_rnk          = r_mth_nat_rnk(i).grs_sls_extnd_rnk,
                   grs_sls_extnd_rnk_pctl     = r_mth_nat_rnk(i).grs_sls_extnd_rnk_pctl * 100,
                   unfrm_qty_ship_rnk         = r_mth_nat_rnk(i).unfrm_qty_ship_rnk,
                   unfrm_qty_ship_rnk_pctl    = r_mth_nat_rnk(i).unfrm_qty_ship_rnk_pctl * 100,
                   grs_tgp_rnk                = r_mth_nat_rnk(i).grs_tgp_rnk,
                   grs_tgp_rnk_pctl           = r_mth_nat_rnk(i).grs_tgp_rnk_pctl * 100,
                   net_acct_rnk               = r_mth_nat_rnk(i).net_acct_rnk,
                   net_acct_rnk_pctl          = r_mth_nat_rnk(i).net_acct_rnk_pctl * 100,
                   net_acct_per_tm_rnk        = r_mth_nat_rnk(i).net_acct_per_tm_rnk,
                   net_acct_per_tm_rnk_pctl   = r_mth_nat_rnk(i).net_acct_per_tm_rnk_pctl * 100,
                   lines_per_drop_rnk         = r_mth_nat_rnk(i).lines_per_drop_rnk,
                   lines_per_drop_rnk_pctl    = r_mth_nat_rnk(i).lines_per_drop_rnk_pctl * 100,
                   eb_pct_rnk                 = r_mth_nat_rnk(i).eb_pct_rnk,
                   eb_pct_rnk_pctl            = r_mth_nat_rnk(i).eb_pct_rnk_pctl * 100
             WHERE fisc_yr_mth = r_mth_nat_rnk(i).fisc_yr_mth
               AND org_lvl = r_mth_nat_rnk(i).org_lvl
               AND org_val = r_mth_nat_rnk(i).org_val
               AND rnk_ctgry = r_mth_nat_rnk(i).rnk_ctgry
               AND cust_clsfctn = r_mth_nat_rnk(i).cust_clsfctn
               AND rnk_by = r_mth_nat_rnk(i).rnk_by
               AND rnk_lvl = 'NATIONAL';
      END LOOP;

      CLOSE c_mth_nat_rnk;

      COMMIT;

      --Update year to date rank values in SLS_RNK_MTH_TMP for org entities ranked by division
      OPEN c_ytd_div_rnk;

      LOOP
         EXIT WHEN c_ytd_div_rnk%NOTFOUND;

         FETCH c_ytd_div_rnk
         BULK COLLECT INTO r_ytd_div_rnk
         LIMIT 50000;

         FORALL i IN 1 .. r_ytd_div_rnk.COUNT()
            UPDATE xdmadm.sls_rnk_mth_tmp
               SET overall_ytd_rnk               = r_ytd_div_rnk(i).overall_ytd_rnk,
                   overall_ytd_rnk_pctl          = r_ytd_div_rnk(i).overall_ytd_rnk_pctl * 100,
                   ir_pct_ytd_rnk                = r_ytd_div_rnk(i).ir_pct_ytd_rnk,
                   ir_pct_ytd_rnk_pctl           = r_ytd_div_rnk(i).ir_pct_ytd_rnk_pctl * 100,
                   grs_sls_extnd_ytd_rnk         = r_ytd_div_rnk(i).grs_sls_extnd_ytd_rnk,
                   grs_sls_extnd_ytd_rnk_pctl    = r_ytd_div_rnk(i).grs_sls_extnd_ytd_rnk_pctl * 100,
                   unfrm_qty_ship_ytd_rnk        = r_ytd_div_rnk(i).unfrm_qty_ship_ytd_rnk,
                   unfrm_qty_ship_ytd_rnk_pctl   = r_ytd_div_rnk(i).unfrm_qty_ship_ytd_rnk_pctl * 100,
                   grs_tgp_ytd_rnk               = r_ytd_div_rnk(i).grs_tgp_ytd_rnk,
                   grs_tgp_ytd_rnk_pctl          = r_ytd_div_rnk(i).grs_tgp_ytd_rnk_pctl * 100
             WHERE fisc_yr_mth = r_ytd_div_rnk(i).fisc_yr_mth
               AND org_lvl = r_ytd_div_rnk(i).org_lvl
               AND org_val = r_ytd_div_rnk(i).org_val
               AND rnk_ctgry = r_ytd_div_rnk(i).rnk_ctgry
               AND cust_clsfctn = r_ytd_div_rnk(i).cust_clsfctn
               AND rnk_by = r_ytd_div_rnk(i).rnk_by
               AND rnk_lvl = 'DIVISION';
      END LOOP;

      CLOSE c_ytd_div_rnk;

      COMMIT;

      --Update year to date rank values in SLS_RNK_MTH_TMP for org entities ranked nationally
      OPEN c_ytd_nat_rnk;

      LOOP
         EXIT WHEN c_ytd_nat_rnk%NOTFOUND;

         FETCH c_ytd_nat_rnk
         BULK COLLECT INTO r_ytd_nat_rnk
         LIMIT 50000;

         FORALL i IN 1 .. r_ytd_nat_rnk.COUNT()
            UPDATE xdmadm.sls_rnk_mth_tmp
               SET overall_ytd_rnk               = r_ytd_nat_rnk(i).overall_ytd_rnk,
                   overall_ytd_rnk_pctl          = r_ytd_nat_rnk(i).overall_ytd_rnk_pctl * 100,
                   ir_pct_ytd_rnk                = r_ytd_nat_rnk(i).ir_pct_ytd_rnk,
                   ir_pct_ytd_rnk_pctl           = r_ytd_nat_rnk(i).ir_pct_ytd_rnk_pctl * 100,
                   grs_sls_extnd_ytd_rnk         = r_ytd_nat_rnk(i).grs_sls_extnd_ytd_rnk,
                   grs_sls_extnd_ytd_rnk_pctl    = r_ytd_nat_rnk(i).grs_sls_extnd_ytd_rnk_pctl * 100,
                   unfrm_qty_ship_ytd_rnk        = r_ytd_nat_rnk(i).unfrm_qty_ship_ytd_rnk,
                   unfrm_qty_ship_ytd_rnk_pctl   = r_ytd_nat_rnk(i).unfrm_qty_ship_ytd_rnk_pctl * 100,
                   grs_tgp_ytd_rnk               = r_ytd_nat_rnk(i).grs_tgp_ytd_rnk,
                   grs_tgp_ytd_rnk_pctl          = r_ytd_nat_rnk(i).grs_tgp_ytd_rnk_pctl * 100
             WHERE fisc_yr_mth = r_ytd_nat_rnk(i).fisc_yr_mth
               AND org_lvl = r_ytd_nat_rnk(i).org_lvl
               AND org_val = r_ytd_nat_rnk(i).org_val
               AND rnk_ctgry = r_ytd_nat_rnk(i).rnk_ctgry
               AND cust_clsfctn = r_ytd_nat_rnk(i).cust_clsfctn
               AND rnk_by = r_ytd_nat_rnk(i).rnk_by
               AND rnk_lvl = 'NATIONAL';
      END LOOP;

      CLOSE c_ytd_nat_rnk;

      COMMIT;
   END upd_sls_rnk_mth_rnk;

   /***********************************************************************
   * Name: UPD_SLS_RNK_ORG_NM
   * Type: Procedure
   * Description: Updates SLS_RNK_ORG and SLS_RKN_ORG_TMP tables with RSM, DSM, and TM names.
   *              This will do initial population of names in the SLS_RNK_ORG_TMP table and
   *              update the names in SLS_RNK_ORG where they have changed.
   *              Uses MAX of name in CUST_CORP since there can be more than one name related
   *              to a single ID.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  ------------------------------------
   *  1.0        06/05/2013  Matt Nicol       Initial Creation
   ************************************************************************/

   PROCEDURE upd_sls_rnk_org_nm IS
      --Cursor to get RSM names that need to be updated in SLS_RNK_ORG
      CURSOR c_rsm_nm IS
         SELECT rgn_sls_mgr_id, rgn_sls_mgr_nm_id
           FROM (  SELECT NVL(cc.rgn_sls_mgr_id, 'UNKNOWN') rgn_sls_mgr_id, MAX(NVL(cc.rgn_sls_mgr_nm_id, 'UNKNOWN' || ' (' || NVL(cc.rgn_sls_mgr_id, 'UNKNOWN') || ')')) rgn_sls_mgr_nm_id
                     FROM xdmadm.cust_corp cc, xdmadm.div_corp d
                    WHERE cc.div_nbr = d.div_nbr
                      AND d.div_typ_cd = 'USF'
                      AND d.cmpny_cd = 1
                      AND d.conv_to_div_nbr IS NULL
                 GROUP BY NVL(cc.rgn_sls_mgr_id, 'UNKNOWN')) cust
          WHERE EXISTS (SELECT 'X'
                          FROM xdmadm.sls_rnk_org
                         WHERE fisc_yr_mth < g_fisc_yr_mth
                           AND rgn_sls_mgr_id = cust.rgn_sls_mgr_id
                           AND NVL(rgn_sls_mgr_nm_id, '-999') <> cust.rgn_sls_mgr_nm_id);

      TYPE t_rsm_nm IS TABLE OF c_rsm_nm%ROWTYPE;

      r_rsm_nm       t_rsm_nm;

      --Cursor to get RSM names that need to be updated in SLS_RNK_ORG_TMP
      CURSOR c_rsm_nm_tmp IS
         SELECT rgn_sls_mgr_id, rgn_sls_mgr_nm_id
           FROM (  SELECT NVL(cc.rgn_sls_mgr_id, 'UNKNOWN') rgn_sls_mgr_id, MAX(NVL(cc.rgn_sls_mgr_nm_id, 'UNKNOWN' || ' (' || NVL(cc.rgn_sls_mgr_id, 'UNKNOWN') || ')')) rgn_sls_mgr_nm_id
                     FROM xdmadm.cust_corp cc, xdmadm.div_corp d
                    WHERE cc.div_nbr = d.div_nbr
                      AND d.div_typ_cd = 'USF'
                      AND d.cmpny_cd = 1
                      AND d.conv_to_div_nbr IS NULL
                 GROUP BY NVL(cc.rgn_sls_mgr_id, 'UNKNOWN')) cust
          WHERE EXISTS (SELECT 'X'
                          FROM xdmadm.sls_rnk_org_tmp
                         WHERE rgn_sls_mgr_id = cust.rgn_sls_mgr_id);

      TYPE t_rsm_nm_tmp IS TABLE OF c_rsm_nm_tmp%ROWTYPE;

      r_rsm_nm_tmp   t_rsm_nm_tmp;

      --Cursor to get DSM names that need to be updated in SLS_RNK_ORG
      CURSOR c_dsm_nm IS
         SELECT dstrct_sls_mgr_id, dstrct_sls_mgr_nm_id
           FROM (  SELECT NVL(cc.dstrct_sls_mgr_id, 'UNKNOWN') dstrct_sls_mgr_id, MAX(NVL(cc.dstrct_sls_mgr_nm_id, 'UNKNOWN' || ' (' || NVL(cc.dstrct_sls_mgr_id, 'UNKNOWN') || ')')) dstrct_sls_mgr_nm_id
                     FROM xdmadm.cust_corp cc, xdmadm.div_corp d
                    WHERE cc.div_nbr = d.div_nbr
                      AND d.div_typ_cd = 'USF'
                      AND d.cmpny_cd = 1
                      AND d.conv_to_div_nbr IS NULL
                 GROUP BY NVL(cc.dstrct_sls_mgr_id, 'UNKNOWN')) cust
          WHERE EXISTS (SELECT 'X'
                          FROM xdmadm.sls_rnk_org
                         WHERE fisc_yr_mth < g_fisc_yr_mth
                           AND dstrct_sls_mgr_id = cust.dstrct_sls_mgr_id
                           AND NVL(dstrct_sls_mgr_nm_id, '-999') <> cust.dstrct_sls_mgr_nm_id);

      TYPE t_dsm_nm IS TABLE OF c_dsm_nm%ROWTYPE;

      r_dsm_nm       t_dsm_nm;

      --Cursor to get DSM names that need to be updated in SLS_RNK_ORG_TMP
      CURSOR c_dsm_nm_tmp IS
         SELECT dstrct_sls_mgr_id, dstrct_sls_mgr_nm_id
           FROM (  SELECT NVL(cc.dstrct_sls_mgr_id, 'UNKNOWN') dstrct_sls_mgr_id, MAX(NVL(cc.dstrct_sls_mgr_nm_id, 'UNKNOWN' || ' (' || NVL(cc.dstrct_sls_mgr_id, 'UNKNOWN') || ')')) dstrct_sls_mgr_nm_id
                     FROM xdmadm.cust_corp cc, xdmadm.div_corp d
                    WHERE cc.div_nbr = d.div_nbr
                      AND d.div_typ_cd = 'USF'
                      AND d.cmpny_cd = 1
                      AND d.conv_to_div_nbr IS NULL
                 GROUP BY NVL(cc.dstrct_sls_mgr_id, 'UNKNOWN')) cust
          WHERE EXISTS (SELECT 'X'
                          FROM xdmadm.sls_rnk_org_tmp
                         WHERE dstrct_sls_mgr_id = cust.dstrct_sls_mgr_id);

      TYPE t_dsm_nm_tmp IS TABLE OF c_dsm_nm_tmp%ROWTYPE;

      r_dsm_nm_tmp   t_dsm_nm_tmp;

      --Cursor to get TM names that need to be updated in SLS_RNK_ORG
      CURSOR c_tm_nm IS
         SELECT trtry_mgr_id, trtry_mgr_nm_id
           FROM (  SELECT NVL(cc.trtry_mgr_id, 'UNKNOWN') trtry_mgr_id, MAX(NVL(cc.trtry_mgr_nm_id, 'UNKNOWN' || ' (' || NVL(cc.trtry_mgr_id, 'UNKNOWN') || ')')) trtry_mgr_nm_id
                     FROM xdmadm.cust_corp cc, xdmadm.div_corp d
                    WHERE cc.div_nbr = d.div_nbr
                      AND d.div_typ_cd = 'USF'
                      AND d.cmpny_cd = 1
                      AND d.conv_to_div_nbr IS NULL
                 GROUP BY NVL(cc.trtry_mgr_id, 'UNKNOWN')) cust
          WHERE EXISTS (SELECT 'X'
                          FROM xdmadm.sls_rnk_org
                         WHERE fisc_yr_mth < g_fisc_yr_mth
                           AND trtry_mgr_id = cust.trtry_mgr_id
                           AND NVL(trtry_mgr_nm_id, '-999') <> cust.trtry_mgr_nm_id);

      TYPE t_tm_nm IS TABLE OF c_tm_nm%ROWTYPE;

      r_tm_nm        t_tm_nm;

      --Cursor to get TM names that need to be updated in SLS_RNK_ORG_TMP
      CURSOR c_tm_nm_tmp IS
         SELECT trtry_mgr_id, trtry_mgr_nm_id
           FROM (  SELECT NVL(cc.trtry_mgr_id, 'UNKNOWN') trtry_mgr_id, MAX(NVL(cc.trtry_mgr_nm_id, 'UNKNOWN' || ' (' || NVL(cc.trtry_mgr_id, 'UNKNOWN') || ')')) trtry_mgr_nm_id
                     FROM xdmadm.cust_corp cc, xdmadm.div_corp d
                    WHERE cc.div_nbr = d.div_nbr
                      AND d.div_typ_cd = 'USF'
                      AND d.cmpny_cd = 1
                      AND d.conv_to_div_nbr IS NULL
                 GROUP BY NVL(cc.trtry_mgr_id, 'UNKNOWN')) cust
          WHERE EXISTS (SELECT 'X'
                          FROM xdmadm.sls_rnk_org_tmp
                         WHERE trtry_mgr_id = cust.trtry_mgr_id);

      TYPE t_tm_nm_tmp IS TABLE OF c_tm_nm_tmp%ROWTYPE;

      r_tm_nm_tmp    t_tm_nm_tmp;
   BEGIN
      -- Update RSM names in SLS_RNK_ORG
      OPEN c_rsm_nm;

      FETCH c_rsm_nm
      BULK COLLECT INTO r_rsm_nm;

      FORALL i IN 1 .. r_rsm_nm.COUNT()
         UPDATE xdmadm.sls_rnk_org
            SET rgn_sls_mgr_nm_id   = r_rsm_nm(i).rgn_sls_mgr_nm_id
          WHERE fisc_yr_mth < g_fisc_yr_mth
            AND rgn_sls_mgr_id = r_rsm_nm(i).rgn_sls_mgr_id
            AND NVL(rgn_sls_mgr_nm_id, '-999') <> r_rsm_nm(i).rgn_sls_mgr_nm_id;

      DBMS_OUTPUT.put_line('Updated RSM names in SLS_RNK_ORG');

      CLOSE c_rsm_nm;

      -- Update RSM names in SLS_RNK_ORG_TMP
      OPEN c_rsm_nm_tmp;

      FETCH c_rsm_nm_tmp
      BULK COLLECT INTO r_rsm_nm_tmp;

      FORALL i IN 1 .. r_rsm_nm_tmp.COUNT()
         UPDATE xdmadm.sls_rnk_org_tmp
            SET rgn_sls_mgr_nm_id   = r_rsm_nm_tmp(i).rgn_sls_mgr_nm_id
          WHERE rgn_sls_mgr_id = r_rsm_nm_tmp(i).rgn_sls_mgr_id;

      DBMS_OUTPUT.put_line('Updated RSM names in SLS_RNK_ORG_TMP');

      CLOSE c_rsm_nm_tmp;

      -- Update DSM names in SLS_RNK_ORG
      OPEN c_dsm_nm;

      FETCH c_dsm_nm
      BULK COLLECT INTO r_dsm_nm;

      FORALL i IN 1 .. r_dsm_nm.COUNT()
         UPDATE xdmadm.sls_rnk_org
            SET dstrct_sls_mgr_nm_id   = r_dsm_nm(i).dstrct_sls_mgr_nm_id
          WHERE fisc_yr_mth < g_fisc_yr_mth
            AND dstrct_sls_mgr_id = r_dsm_nm(i).dstrct_sls_mgr_id
            AND NVL(dstrct_sls_mgr_nm_id, '-999') <> r_dsm_nm(i).dstrct_sls_mgr_nm_id;

      DBMS_OUTPUT.put_line('Updated DSM names in SLS_RNK_ORG');

      CLOSE c_dsm_nm;

      -- Update DSM names in SLS_RNK_ORG_TMP
      OPEN c_dsm_nm_tmp;

      FETCH c_dsm_nm_tmp
      BULK COLLECT INTO r_dsm_nm_tmp;

      FORALL i IN 1 .. r_dsm_nm_tmp.COUNT()
         UPDATE xdmadm.sls_rnk_org_tmp
            SET dstrct_sls_mgr_nm_id   = r_dsm_nm_tmp(i).dstrct_sls_mgr_nm_id
          WHERE dstrct_sls_mgr_id = r_dsm_nm_tmp(i).dstrct_sls_mgr_id;

      DBMS_OUTPUT.put_line('Updated DSM names in SLS_RNK_ORG_TMP');

      CLOSE c_dsm_nm_tmp;

      -- Update TM names in SLS_RNK_ORG
      OPEN c_tm_nm;

      FETCH c_tm_nm
      BULK COLLECT INTO r_tm_nm;

      FORALL i IN 1 .. r_tm_nm.COUNT()
         UPDATE xdmadm.sls_rnk_org
            SET trtry_mgr_nm_id   = r_tm_nm(i).trtry_mgr_nm_id
          WHERE fisc_yr_mth < g_fisc_yr_mth
            AND trtry_mgr_id = r_tm_nm(i).trtry_mgr_id
            AND NVL(trtry_mgr_nm_id, '-999') <> r_tm_nm(i).trtry_mgr_nm_id;

      DBMS_OUTPUT.put_line('Updated TM names in SLS_RNK_ORG');

      CLOSE c_tm_nm;

      -- Update TM names in SLS_RNK_ORG_TMP
      OPEN c_tm_nm_tmp;

      FETCH c_tm_nm_tmp
      BULK COLLECT INTO r_tm_nm_tmp;

      FORALL i IN 1 .. r_tm_nm_tmp.COUNT()
         UPDATE xdmadm.sls_rnk_org_tmp
            SET trtry_mgr_nm_id   = r_tm_nm_tmp(i).trtry_mgr_nm_id
          WHERE trtry_mgr_id = r_tm_nm_tmp(i).trtry_mgr_id;

      DBMS_OUTPUT.put_line('Updated TM names in SLS_RNK_ORG_TMP');

      CLOSE c_tm_nm_tmp;

      COMMIT;
   END upd_sls_rnk_org_nm;
END pkg_sls_rnk;
/




  CREATE OR REPLACE PACKAGE "XDMADM"."PKG_SLS_PYR" AS
   /***********************************************************************
   * Name: pkg_sls_pyr
   * Type: Package
   * Description: Set of help functions for pyramid sales
                   aggregate tables in XDMADM.
   *  REVISIONS:
   *  Ver        Date        Author                 Description
   *  ---------  ----------  ---------------        ------------------------------------
   *  1.0        12/10/2011  Sandeep Manocha        Created this procedure
   *  1.1        01/20/2012  Manisha Singh          Modified for Executive Summary 1.2.4 changes - Pyramid Projection load as follows:
                                                     i) Added new columns - cust_cntrct_flg,cust_natl_mngd_flg,grs_tgp, net_sls, net_tgp in t_aggr_dim,t_aggr_msr and t_aggr_growth RECORD
   *  1.2        07/05/2012  Matt Nicol             Updated to include PAs
   *  1.3        01/09/2013  Madhu Veerapaneni      Added new column CUST_ACCTBLTY_FLG from XDMADM.CUST_CORP to SALES_SUMM_PYR_PRJ tables
   *  1.4        06/17/2013  Madhu Veerapaneni      Added new variables for Merchandising aggregate tables
   *  1.5        02/27/2014  Ankur Patel            Support for two projections synonyms; additional fields including NET_NON_PROD_ALWNC, NET_PROD_ALWNC, RBT_LOC_ACCRL_AMT,
   * 												RBT_NATL_ACCRL_AMT, RBT_LOC_EXP_AMT, RBT_NATL_EXP_AMT, NET_PROD_CHRG, NET_NON_PROD_CHRG
   ************************************************************************/
   c_aggr_syn_nm   		CONSTANT VARCHAR2(30) := 'SALES_SUMM_PYR_AGGR';
   c_prj_load_syn_nm	CONSTANT VARCHAR2(30) := 'SALES_SUMM_PYR_PRJ_LOAD';
   c_schema_nm     		CONSTANT VARCHAR2(8) := 'XDMADM';
   c_success       		CONSTANT NUMBER := 0;
   c_error         		CONSTANT NUMBER := 1;

   --Added for Sales Merchandising Scorecard Aggr tables
   c_merch_syn_nm         CONSTANT VARCHAR2(30) := 'SALES_SUMM_MRCH_AGGR';
   c_cust_merch_syn_nm    CONSTANT VARCHAR2(30) := 'CUST_CORP_MRCH_AGGR';
   c_pim_merch_syn_nm     CONSTANT VARCHAR2(30) := 'PIM_CORP_MRCH_AGGR';

   --Added for Exec. Summary Dimension tables
   c_cust_pyr_syn_nm      CONSTANT VARCHAR2(30) := 'CUST_CORP_PYR_AGGR';
   c_pim_pyr_syn_nm       CONSTANT VARCHAR2(30) := 'PIM_CORP_AGGR';
   --

   TYPE t_aggr_dim IS RECORD(div_nbr               sales_summ_pyr_aggr_a.div_nbr%TYPE,
                             pim_cls_id            sales_summ_pyr_aggr_a.pim_cls_id%TYPE,
                             pim_suprcls_id_crnt   sales_summ_pyr_aggr_a.pim_suprcls_id_crnt%TYPE,
                             pim_suprcls_id_actl   sales_summ_pyr_aggr_a.pim_suprcls_id_actl%TYPE,
                             trd_cls               sales_summ_pyr_aggr_a.trd_cls%TYPE,
                             pyr_seg_cd            sales_summ_pyr_aggr_a.pyr_seg_cd%TYPE,
                             corp_mlt_unit_nbr     sales_summ_pyr_aggr_a.corp_mlt_unit_nbr%TYPE,
                             prnt_mlt_unit_cd      sales_summ_pyr_aggr_a.prnt_mlt_unit_cd%TYPE,
                             pim_brnd_typ          sales_summ_pyr_aggr_a.pim_brnd_typ%TYPE,
                             --Added for Executive Summary 1.2.4 changes
                             cust_cntrct_flg       sales_summ_pyr_aggr_a.cust_cntrct_flg%TYPE,
                             cust_natl_mngd_flg    sales_summ_pyr_aggr_a.cust_natl_mngd_flg%TYPE,
                             cust_acctblty_flg     sales_summ_pyr_aggr_a.cust_acctblty_flg%TYPE,  --Fallout 1.3 changes
                             sls_revenue_div_nbr   sales_summ_pyr_aggr_a.sls_revenue_div_nbr%TYPE
							 );

   TYPE t_aggr_msr IS RECORD(sales     				sales_summ_pyr_aggr_a.grs_sls_extnd%TYPE,
                             cases     				sales_summ_pyr_aggr_a.unfrm_qty_ship%TYPE,
                             tagp      				sales_summ_pyr_aggr_a.actl_gp%TYPE,
                             pa        				sales_summ_pyr_aggr_a.ttl_alwnc%TYPE,
                             --Added for Executive Summary 1.2.4 changes
                             grs_tgp   				sales_summ_pyr_aggr_a.grs_tgp%TYPE,
                             net_sls   				sales_summ_pyr_aggr_a.net_sls%TYPE,
                             net_tgp   				sales_summ_pyr_aggr_a.net_tgp%TYPE,
							 --Added for ES 4.0 changes
							 net_non_prod_alwnc	    sales_summ_pyr_aggr_a.net_non_prod_alwnc%TYPE,
							 net_prod_alwnc        	sales_summ_pyr_aggr_a.net_prod_alwnc%TYPE,
							 rbt_loc_accrl_amt     	sales_summ_pyr_aggr_a.rbt_loc_accrl_amt%TYPE,
							 rbt_natl_accrl_amt    	sales_summ_pyr_aggr_a.rbt_natl_accrl_amt%TYPE,
							 rbt_loc_exp_amt       	sales_summ_pyr_aggr_a.rbt_loc_exp_amt%TYPE,
							 rbt_natl_exp_amt      	sales_summ_pyr_aggr_a.rbt_natl_exp_amt%TYPE,
							 net_prod_chrg         	sales_summ_pyr_aggr_a.net_prod_chrg%TYPE,
							 net_non_prod_chrg     	sales_summ_pyr_aggr_a.net_non_prod_chrg%TYPE,
							 ec_grs_sls_extnd       sales_summ_pyr_aggr_a.ec_grs_sls_extnd%TYPE,
							 py_cust_cases      	sales_summ_pyr_aggr_a.py_cust_cases%TYPE,
							 py_cust_cnt         	sales_summ_pyr_aggr_a.py_cust_cnt%TYPE,
							 cust_bow_cnt     		sales_summ_pyr_aggr_a.cust_bow_cnt%TYPE
							 );

   TYPE t_aggr_growth IS RECORD(sales     				NUMBER(13, 4),
                                cases     				NUMBER(13, 4),
                                tagp      				NUMBER(13, 4),
                                pa        				NUMBER(13, 4),
                                grs_tgp   				NUMBER(13, 4),
                                net_sls   				NUMBER(13, 4),
                                net_tgp				 	NUMBER(13, 4),
								net_non_prod_alwnc		NUMBER(11, 4),
								net_prod_alwnc    		NUMBER(11, 4),
								rbt_loc_accrl_amt     	NUMBER(17, 8),
								rbt_natl_accrl_amt    	NUMBER(17, 8),
								rbt_loc_exp_amt       	NUMBER(17, 8),
								rbt_natl_exp_amt      	NUMBER(17, 8),
								net_prod_chrg         	NUMBER(11, 4),
								net_non_prod_chrg     	NUMBER(11, 4),
								ec_grs_sls_extnd        NUMBER(11, 4),
								py_cust_cases      		NUMBER(11, 4),
								py_cust_cnt         	NUMBER(11),
								cust_bow_cnt     		NUMBER(11)
							 );
                                --Added for Executive Summary 1.2.4 changes

   FUNCTION get_fisc_wk(p_clndr_dt IN DATE)
      RETURN VARCHAR2;

   FUNCTION get_measures(p_wk_from      IN VARCHAR2,
                         p_wk_to        IN VARCHAR2,
                         p_aggr_dim     IN t_aggr_dim)
      RETURN t_aggr_msr;

   FUNCTION get_wow_growth(p_wk_from      IN VARCHAR2,
                           p_wk_to        IN VARCHAR2,
                           p_aggr_dim     IN t_aggr_dim)
      RETURN t_aggr_growth;

   FUNCTION get_msr_growth(p_msr_cy       IN NUMBER,
                           p_msr_ly       IN NUMBER)
      RETURN NUMBER;

   FUNCTION get_msr_prj(p_msr          IN t_aggr_msr,
                        p_msr_growth   IN t_aggr_growth)
      RETURN t_aggr_msr;

   FUNCTION fn_get_prev_yr_wk(p_fisc_yr_wk    VARCHAR,
                              p_prev_yr       NUMBER)
      RETURN VARCHAR2
      DETERMINISTIC;

   FUNCTION fn_get_tbl_nm_syn(p_syn_nm        VARCHAR2)
      RETURN VARCHAR2;

   FUNCTION fn_get_tbl_nm(p_syn_nm        VARCHAR2,
                          p_tbl_actv      VARCHAR2)
      RETURN VARCHAR2;

   PROCEDURE set_syn_tbl(p_tbl_nm        VARCHAR2,
                         p_tbl_actv      VARCHAR2);
END pkg_sls_pyr;
/
CREATE OR REPLACE PACKAGE BODY "XDMADM"."PKG_SLS_PYR" AS
   /***********************************************************************
   * Name: PKG_SLS_PYR
   * Type: Package Body
   * Description: Set of help functions for corporate pyramid sales
                  aggregate and projection tables in XDMADM.
   *  REVISIONS:
   *  Ver        Date        Author             Description
   *  ---------  ----------  ---------------    ------------------------------------
   *  1.0        10/12/2011  Sandeep Manocha    1. Created this procedure.
   *  1.1        01/20/2012  Manisha Singh      Modified for Executive Summary 1.2.4 changes - Pyramid Projection load.
                                                                 Added new columns - cust_cntrct_flg,cust_natl_mngd_flg,grs_tgp, net_sls, net_tgp.
   *  1.2        02/29/2012  Wes Holbert        Adding missing code for WOW related to new measures    grs_tgp, net_sls, net_tgp.
   *  1.3        01/09/2013  Madhu Veerapaneni  Added new column CUST_ACCTBLTY_FLG from XDMADM.CUST_CORP to SALES_SUMM_PYR_PRJ tables
   *  1.4        02/27/2014  Ankur Patel        Incorporating new synonyms; removing dynamic SQL; calculations for new columns
   *                                            New function to pull table name for a synonym
   *  1.5        06/23/2014  Matt Nicol         Updated FN_GET_PREV_YR_WK to fix calendar bug
   ************************************************************************/
   TYPE t_wow_wks IS TABLE OF xdmadm.time_corp.fisc_yr_wk%TYPE
                        INDEX BY BINARY_INTEGER;

   r_wow_wks       t_wow_wks;

   l_wow_prev_wk   VARCHAR2(6);

   /***********************************************************************
   * Name: GET_FISC_WK
   * Type: Function
   * Description: Return fiscal week for a given calendar date
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        10/12/2011  Sandeep Manocha  1. Created this procedure.
   ************************************************************************/
   FUNCTION get_fisc_wk(p_clndr_dt IN DATE)
      RETURN VARCHAR2 IS
      l_fisc_yr_wk   VARCHAR2(6);
   BEGIN
      SELECT fisc_yr_wk
        INTO l_fisc_yr_wk
        FROM time_corp
       WHERE TRUNC(clndr_dt) = TRUNC(p_clndr_dt);

      RETURN l_fisc_yr_wk;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in GET_FISC_WK');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END get_fisc_wk;

   /***********************************************************************
   * Name: GET_MSR_PRJ
   * Type: Function
   * Description: Determine projected measures based on given
                  measures and growth.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        10/12/2011  Sandeep Manocha  1. Created this procedure.
   *  1.1        01/20/2012  Manisha Singh      1. Added new measures - grs_tgp, net_sls, net_tgp
   ************************************************************************/
   FUNCTION get_msr_prj(p_msr          IN t_aggr_msr,
                        p_msr_growth   IN t_aggr_growth)
      RETURN t_aggr_msr IS
      r_aggr_msr   t_aggr_msr;
   BEGIN
      r_aggr_msr.sales                := ROUND(p_msr.sales * (1 + p_msr_growth.sales), 2);
      r_aggr_msr.cases                := ROUND(p_msr.cases * (1 + p_msr_growth.cases), 2);
      r_aggr_msr.tagp                 := ROUND(p_msr.tagp * (1 + p_msr_growth.tagp), 2);
      r_aggr_msr.pa                   := ROUND(p_msr.pa * (1 + p_msr_growth.pa), 2);
      --Added new measures - grs_tgp, net_sls, net_tgp  for Executive Summary 1.2.4
      r_aggr_msr.grs_tgp              := ROUND(p_msr.grs_tgp * (1 + p_msr_growth.grs_tgp), 2);
      r_aggr_msr.net_sls              := ROUND(p_msr.net_sls * (1 + p_msr_growth.net_sls), 2);
      r_aggr_msr.net_tgp              := ROUND(p_msr.net_tgp * (1 + p_msr_growth.net_tgp), 2);
      --Added new measures - net_non_prod_alwnc, net_prod_alwnc, rbt_loc_accrl_amt, rbt_natl_accrl_amt, rbt_loc_exp_amt, rbt_natl_exp_amt, net_prod_chrg, net_non_prod_chrg
      r_aggr_msr.net_non_prod_alwnc   := ROUND(p_msr.net_non_prod_alwnc * (1 + p_msr_growth.net_non_prod_alwnc), 2);
      r_aggr_msr.net_prod_alwnc       := ROUND(p_msr.net_prod_alwnc * (1 + p_msr_growth.net_prod_alwnc), 2);
      r_aggr_msr.rbt_loc_accrl_amt    := ROUND(p_msr.rbt_loc_accrl_amt * (1 + p_msr_growth.rbt_loc_accrl_amt), 2);
      r_aggr_msr.rbt_natl_accrl_amt   := ROUND(p_msr.rbt_natl_accrl_amt * (1 + p_msr_growth.rbt_natl_accrl_amt), 2);
      r_aggr_msr.rbt_loc_exp_amt      := ROUND(p_msr.rbt_loc_exp_amt * (1 + p_msr_growth.rbt_loc_exp_amt), 2);
      r_aggr_msr.rbt_natl_exp_amt     := ROUND(p_msr.rbt_natl_exp_amt * (1 + p_msr_growth.rbt_natl_exp_amt), 2);
      r_aggr_msr.net_prod_chrg        := ROUND(p_msr.net_prod_chrg * (1 + p_msr_growth.net_prod_chrg), 2);
      r_aggr_msr.net_non_prod_chrg    := ROUND(p_msr.net_non_prod_chrg * (1 + p_msr_growth.net_non_prod_chrg), 2);
      r_aggr_msr.ec_grs_sls_extnd     := ROUND(p_msr.ec_grs_sls_extnd * (1 + p_msr_growth.ec_grs_sls_extnd), 2);
      r_aggr_msr.py_cust_cases        := ROUND(p_msr.py_cust_cases * (1 + p_msr_growth.py_cust_cases), 2);
      r_aggr_msr.py_cust_cnt          := ROUND(p_msr.py_cust_cnt * (1 + p_msr_growth.py_cust_cnt), 2);
      r_aggr_msr.cust_bow_cnt         := ROUND(p_msr.cust_bow_cnt * (1 + p_msr_growth.cust_bow_cnt), 2);

      RETURN r_aggr_msr;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in GET_MSR_PRJ');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END get_msr_prj;

   /***********************************************************************
   * Name: GET_MEASURES
   * Type: Function
   * Description: Calculate measures for given dimensions for given weeks range
   *  REVISIONS:
   *  Ver        Date        Author            Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        10/12/2011  Sandeep Manocha    1. Created this procedure.
   *  1.1        01/20/2012  Manisha Singh      1. Added new columns for Executive Summ 1.2.4
   *  1.2        02/11/2014  Madhu Veerapaneni  1.Modified to get the Active pyramid sls table
   *                                              instead of the inactive pyramid sls table so
   *                                              the projections can run after the synonym swap of
   *                                              pyramid sales table
   *  1.3        03/27/2014  Ankur Patel        1. Removed ly_wtd='N' condition from WHERE clause
   ************************************************************************/
   FUNCTION get_measures(p_wk_from      IN VARCHAR2,
                         p_wk_to        IN VARCHAR2,
                         p_aggr_dim     IN t_aggr_dim)
      RETURN t_aggr_msr IS
      r_aggr_msr   t_aggr_msr;
   BEGIN
      SELECT /*+ use_invisible_indexes */
            NVL(SUM(grs_sls_extnd), 0) sales,
             NVL(SUM(unfrm_qty_ship), 0) cases,
             NVL(SUM(actl_gp), 0) tagp,
             NVL(SUM(ttl_alwnc), 0) pa,
             NVL(SUM(grs_tgp), 0) grs_tgp,
             NVL(SUM(net_sls), 0) net_sls,
             NVL(SUM(net_tgp), 0) net_tgp,
             NVL(SUM(net_non_prod_alwnc), 0) net_non_prod_alwnc,
             NVL(SUM(net_prod_alwnc), 0) net_prod_alwnc,
             NVL(SUM(rbt_loc_accrl_amt), 0) rbt_loc_accrl_amt,
             NVL(SUM(rbt_natl_accrl_amt), 0) rbt_natl_accrl_amt,
             NVL(SUM(rbt_loc_exp_amt), 0) rbt_loc_exp_amt,
             NVL(SUM(rbt_natl_exp_amt), 0) rbt_natl_exp_amt,
             NVL(SUM(net_prod_chrg), 0) net_prod_chrg,
             NVL(SUM(net_non_prod_chrg), 0) net_non_prod_chrg,
             NVL(SUM(ec_grs_sls_extnd), 0) ec_grs_sls_extnd,
             NVL(SUM(py_cust_cases), 0) py_cust_cases,
             NVL(SUM(py_cust_cnt), 0) py_cust_cnt,
             NVL(SUM(cust_bow_cnt), 0) cust_bow_cnt
        INTO r_aggr_msr.sales,
             r_aggr_msr.cases,
             r_aggr_msr.tagp,
             r_aggr_msr.pa,
             --Added new measures for Executive Summ 1.2.4
             r_aggr_msr.grs_tgp,
             r_aggr_msr.net_sls,
             r_aggr_msr.net_tgp,
             --Added new measures for Executive Summ 4.0
             r_aggr_msr.net_non_prod_alwnc,
             r_aggr_msr.net_prod_alwnc,
             r_aggr_msr.rbt_loc_accrl_amt,
             r_aggr_msr.rbt_natl_accrl_amt,
             r_aggr_msr.rbt_loc_exp_amt,
             r_aggr_msr.rbt_natl_exp_amt,
             r_aggr_msr.net_prod_chrg,
             r_aggr_msr.net_non_prod_chrg,
             r_aggr_msr.ec_grs_sls_extnd,
             r_aggr_msr.py_cust_cases,
             r_aggr_msr.py_cust_cnt,
             r_aggr_msr.cust_bow_cnt
        FROM xdmadm.sales_summ_pyr_aggr
       WHERE fisc_yr_wk BETWEEN p_wk_from AND p_wk_to
         AND div_nbr = p_aggr_dim.div_nbr
         AND pim_cls_id = p_aggr_dim.pim_cls_id
         AND pim_suprcls_id_crnt = p_aggr_dim.pim_suprcls_id_crnt
         AND pim_suprcls_id_actl = p_aggr_dim.pim_suprcls_id_actl
         AND trd_cls = p_aggr_dim.trd_cls
         AND pyr_seg_cd = p_aggr_dim.pyr_seg_cd
         AND corp_mlt_unit_nbr = p_aggr_dim.corp_mlt_unit_nbr
         AND prnt_mlt_unit_cd = p_aggr_dim.prnt_mlt_unit_cd
         AND pim_brnd_typ = p_aggr_dim.pim_brnd_typ
         AND cust_cntrct_flg = p_aggr_dim.cust_cntrct_flg
         AND cust_natl_mngd_flg = p_aggr_dim.cust_natl_mngd_flg
         AND cust_acctblty_flg = p_aggr_dim.cust_acctblty_flg
         AND sls_revenue_div_nbr = p_aggr_dim.sls_revenue_div_nbr;

      RETURN r_aggr_msr;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in GET_MEASURES');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END get_measures;

   /***********************************************************************
   * Name: GET_WOW_GROWTH
   * Type: Function
   * Description: Loop through previous weeks to determent WoW growth for
                  each week and then average the results
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        10/12/2011  Sandeep Manocha  1. Created this procedure.
   *  1.1        01/20/2012  Manisha Singh       1. Added new columns for Executive Summ 1.2.4
   ************************************************************************/
   FUNCTION get_wow_growth(p_wk_from      IN VARCHAR2,
                           p_wk_to        IN VARCHAR2,
                           p_aggr_dim     IN t_aggr_dim)
      RETURN t_aggr_growth IS
      l_prev_wk        VARCHAR2(6);
      r_total_growth   t_aggr_growth;
      r_avg_growth     t_aggr_growth;
      l_wk_cnt         NUMBER := 0;

      CURSOR c_wow_wks(x_first_wk     IN VARCHAR2,
                       x_last_wk      IN VARCHAR2) IS
           SELECT fisc_yr_wk
             FROM wkly_time_corp
            WHERE fisc_yr_wk BETWEEN x_first_wk AND x_last_wk
         ORDER BY fisc_yr_wk;

      r_cur_wk_msr     t_aggr_msr;
      r_prev_wk_msr    t_aggr_msr;
   BEGIN
      IF (l_wow_prev_wk IS NULL) THEN
         SELECT fisc_yr_wkago
           INTO l_wow_prev_wk
           FROM wkly_time_corp
          WHERE fisc_yr_wk = p_wk_from;

         --Fetch weeks
         OPEN c_wow_wks(p_wk_from, p_wk_to);

         FETCH c_wow_wks
         BULK COLLECT INTO r_wow_wks;

         CLOSE c_wow_wks;
      END IF;

      r_prev_wk_msr                       := get_measures(l_wow_prev_wk, l_wow_prev_wk, p_aggr_dim);

      r_total_growth.sales                := 0;
      r_total_growth.cases                := 0;
      r_total_growth.tagp                 := 0;
      r_total_growth.pa                   := 0;
      --  Added new columns for Executive Summ 1.2.4
      r_total_growth.grs_tgp              := 0;
      r_total_growth.net_sls              := 0;
      r_total_growth.net_tgp              := 0;
      --  Added new columns for Executive Summ 4.0
      r_total_growth.net_non_prod_alwnc   := 0;
      r_total_growth.net_prod_alwnc       := 0;
      r_total_growth.rbt_loc_accrl_amt    := 0;
      r_total_growth.rbt_natl_accrl_amt   := 0;
      r_total_growth.rbt_loc_exp_amt      := 0;
      r_total_growth.rbt_natl_exp_amt     := 0;
      r_total_growth.net_prod_chrg        := 0;
      r_total_growth.net_non_prod_chrg    := 0;
      r_total_growth.ec_grs_sls_extnd     := 0;
      r_total_growth.py_cust_cases        := 0;
      r_total_growth.py_cust_cnt          := 0;
      r_total_growth.cust_bow_cnt         := 0;

      --

      FOR i IN r_wow_wks.FIRST .. r_wow_wks.LAST LOOP
         r_cur_wk_msr                        := get_measures(r_wow_wks(i), r_wow_wks(i), p_aggr_dim);

         r_total_growth.sales                := r_total_growth.sales + get_msr_growth(r_cur_wk_msr.sales, r_prev_wk_msr.sales);
         r_total_growth.cases                := r_total_growth.cases + get_msr_growth(r_cur_wk_msr.cases, r_prev_wk_msr.cases);
         r_total_growth.tagp                 := r_total_growth.tagp + get_msr_growth(r_cur_wk_msr.tagp, r_prev_wk_msr.tagp);
         r_total_growth.pa                   := r_total_growth.pa + get_msr_growth(r_cur_wk_msr.pa, r_prev_wk_msr.pa);
         --  Added new columns for Executive Summ 1.2.4
         r_total_growth.grs_tgp              := r_total_growth.grs_tgp + get_msr_growth(r_cur_wk_msr.grs_tgp, r_prev_wk_msr.grs_tgp);
         r_total_growth.net_sls              := r_total_growth.net_sls + get_msr_growth(r_cur_wk_msr.net_sls, r_prev_wk_msr.net_sls);
         r_total_growth.net_tgp              := r_total_growth.net_tgp + get_msr_growth(r_cur_wk_msr.net_tgp, r_prev_wk_msr.net_tgp);
         --  Added new columns for Executive Summ 4.0
         r_total_growth.net_non_prod_alwnc   := r_total_growth.net_non_prod_alwnc + get_msr_growth(r_cur_wk_msr.net_non_prod_alwnc, r_prev_wk_msr.net_non_prod_alwnc);
         r_total_growth.net_prod_alwnc       := r_total_growth.net_prod_alwnc + get_msr_growth(r_cur_wk_msr.net_prod_alwnc, r_prev_wk_msr.net_prod_alwnc);
         r_total_growth.rbt_loc_accrl_amt    := r_total_growth.rbt_loc_accrl_amt + get_msr_growth(r_cur_wk_msr.rbt_loc_accrl_amt, r_prev_wk_msr.rbt_loc_accrl_amt);
         r_total_growth.rbt_natl_accrl_amt   := r_total_growth.rbt_natl_accrl_amt + get_msr_growth(r_cur_wk_msr.rbt_natl_accrl_amt, r_prev_wk_msr.rbt_natl_accrl_amt);
         r_total_growth.rbt_loc_exp_amt      := r_total_growth.rbt_loc_exp_amt + get_msr_growth(r_cur_wk_msr.rbt_loc_exp_amt, r_prev_wk_msr.rbt_loc_exp_amt);
         r_total_growth.rbt_natl_exp_amt     := r_total_growth.rbt_natl_exp_amt + get_msr_growth(r_cur_wk_msr.rbt_natl_exp_amt, r_prev_wk_msr.rbt_natl_exp_amt);
         r_total_growth.net_prod_chrg        := r_total_growth.net_prod_chrg + get_msr_growth(r_cur_wk_msr.net_prod_chrg, r_prev_wk_msr.net_prod_chrg);
         r_total_growth.net_non_prod_chrg    := r_total_growth.net_non_prod_chrg + get_msr_growth(r_cur_wk_msr.net_non_prod_chrg, r_prev_wk_msr.net_non_prod_chrg);
         r_total_growth.ec_grs_sls_extnd     := r_total_growth.ec_grs_sls_extnd + get_msr_growth(r_cur_wk_msr.ec_grs_sls_extnd, r_prev_wk_msr.ec_grs_sls_extnd);
         r_total_growth.py_cust_cases        := r_total_growth.py_cust_cases + get_msr_growth(r_cur_wk_msr.py_cust_cases, r_prev_wk_msr.py_cust_cases);
         r_total_growth.py_cust_cnt          := r_total_growth.py_cust_cnt + get_msr_growth(r_cur_wk_msr.py_cust_cnt, r_prev_wk_msr.py_cust_cnt);
         r_total_growth.cust_bow_cnt         := r_total_growth.cust_bow_cnt + get_msr_growth(r_cur_wk_msr.cust_bow_cnt, r_prev_wk_msr.cust_bow_cnt);
         --
         r_prev_wk_msr                       := r_cur_wk_msr;
         l_wk_cnt                            := l_wk_cnt + 1;
      END LOOP;

      r_avg_growth.sales                  := r_total_growth.sales / l_wk_cnt;
      r_avg_growth.cases                  := r_total_growth.cases / l_wk_cnt;
      r_avg_growth.tagp                   := r_total_growth.tagp / l_wk_cnt;
      r_avg_growth.pa                     := r_total_growth.pa / l_wk_cnt;
      r_avg_growth.grs_tgp                := r_total_growth.grs_tgp / l_wk_cnt; --Change 1.2
      r_avg_growth.net_sls                := r_total_growth.net_sls / l_wk_cnt; --Change 1.2
      r_avg_growth.net_tgp                := r_total_growth.net_tgp / l_wk_cnt; --Change 1.2
      r_avg_growth.net_non_prod_alwnc     := r_total_growth.net_non_prod_alwnc / l_wk_cnt; -- Change ES 4.0
      r_avg_growth.net_prod_alwnc         := r_total_growth.net_prod_alwnc / l_wk_cnt; -- Change ES 4.0
      r_avg_growth.rbt_loc_accrl_amt      := r_total_growth.rbt_loc_accrl_amt / l_wk_cnt; -- Change ES 4.0
      r_avg_growth.rbt_natl_accrl_amt     := r_total_growth.rbt_natl_accrl_amt / l_wk_cnt; -- Change ES 4.0
      r_avg_growth.rbt_loc_exp_amt        := r_total_growth.rbt_loc_exp_amt / l_wk_cnt; -- Change ES 4.0
      r_avg_growth.rbt_natl_exp_amt       := r_total_growth.rbt_natl_exp_amt / l_wk_cnt; -- Change ES 4.0
      r_avg_growth.net_prod_chrg          := r_total_growth.net_prod_chrg / l_wk_cnt; -- Change ES 4.0
      r_avg_growth.net_non_prod_chrg      := r_total_growth.net_non_prod_chrg / l_wk_cnt; -- Change ES 4.0
      r_avg_growth.ec_grs_sls_extnd       := r_total_growth.ec_grs_sls_extnd / l_wk_cnt;
      r_avg_growth.py_cust_cases          := r_total_growth.py_cust_cases / l_wk_cnt;
      r_avg_growth.py_cust_cnt            := r_total_growth.py_cust_cnt / l_wk_cnt;
      r_avg_growth.cust_bow_cnt           := r_total_growth.cust_bow_cnt / l_wk_cnt;

      RETURN r_avg_growth;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in GET_WOW_GROWTH');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END get_wow_growth;

   /***********************************************************************
   * Name: GET_MSR_GROWTH
   * Type: Function
   * Description: Calculate growth given two measures. The growth is capped at
                  200% to prevent anomolies.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        10/12/2011  Sandeep Manocha  1. Created this procedure.
   ************************************************************************/
   FUNCTION get_msr_growth(p_msr_cy       IN NUMBER,
                           p_msr_ly       IN NUMBER)
      RETURN NUMBER IS
      l_growth   NUMBER;
   BEGIN
      IF (p_msr_ly <> 0) THEN
         l_growth   := ROUND((p_msr_cy - p_msr_ly) / p_msr_ly, 4);

         IF (l_growth > 2) THEN
            l_growth   := 2;
         END IF;

         IF (l_growth < -2) THEN
            l_growth   := -2;
         END IF;
      ELSE
         l_growth   := 0;
      END IF;

      RETURN l_growth;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in GET_MSR_GROWTH');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END get_msr_growth;

   /***********************************************************************
   * Name: FN_GET_PREV_YR_WK - OLD
   * Type: Function
   * Description: Calculate corresponding fiscal week in previous year for
                  given fiscal week and number of years.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        10/12/2011  Sandeep Manocha  1. Created this procedure.
   ************************************************************************/
   /*
      FUNCTION fn_get_prev_yr_wk(p_fisc_yr_wk    VARCHAR,
                                 p_prev_yr       NUMBER)
         RETURN VARCHAR2
         DETERMINISTIC IS
         l_prev_fisc_yr_wk   VARCHAR2(6);
         l_prev_yr           VARCHAR2(4);
         l_fisc_wk_of_yr     VARCHAR2(2);
         l_fisc_yr           NUMBER;
         l_wks_in_prev_yr    NUMBER;
      BEGIN
         l_fisc_yr         := SUBSTR(p_fisc_yr_wk, 1, 4);
         l_prev_yr         := l_fisc_yr - p_prev_yr;
         l_fisc_wk_of_yr   := SUBSTR(p_fisc_yr_wk, 5, 2);

         SELECT fisc_wk_of_yr
           INTO l_wks_in_prev_yr
           FROM xdmadm.time_corp
          WHERE clndr_dt = l_prev_yr || '1231';

         IF (l_fisc_wk_of_yr = 53) THEN
            l_prev_fisc_yr_wk   := l_prev_yr || '52';
         ELSIF (l_fisc_wk_of_yr BETWEEN 49 AND 52) THEN
            IF (l_wks_in_prev_yr = 53) THEN
               l_prev_fisc_yr_wk   := l_prev_yr || TO_CHAR(l_fisc_wk_of_yr + 1);
            ELSE
               l_prev_fisc_yr_wk   := l_prev_yr || l_fisc_wk_of_yr;
            END IF;
         ELSE
            l_prev_fisc_yr_wk   := l_prev_yr || l_fisc_wk_of_yr;
         END IF;

         RETURN l_prev_fisc_yr_wk;
      EXCEPTION
         WHEN OTHERS THEN
            DBMS_OUTPUT.put_line('Error in FN_GET_PREV_YR_WK');
            DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
      END fn_get_prev_yr_wk;
   */

   /***********************************************************************
   * Name: FN_GET_PREV_YR_WK - NEW
   * Type: Function
   * Description: Calculate corresponding fiscal week in previous year for
                  given fiscal week and number of years.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        06/23/2014  Matt Nicol       Rewrite due to fiscal calendar bug
   ************************************************************************/
   FUNCTION fn_get_prev_yr_wk(p_fisc_yr_wk    VARCHAR,
                              p_prev_yr       NUMBER)
      RETURN VARCHAR2
      DETERMINISTIC IS
      l_prev_fisc_yr_wk   VARCHAR2(6);
   BEGIN
      l_prev_fisc_yr_wk   := p_fisc_yr_wk;

      FOR i IN 1 .. p_prev_yr LOOP
         SELECT fisc_yrago_wk
           INTO l_prev_fisc_yr_wk
           FROM xdmadm.wkly_time_corp
          WHERE fisc_yr_wk = l_prev_fisc_yr_wk;
      END LOOP;

      RETURN l_prev_fisc_yr_wk;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in FN_GET_PREV_YR_WK');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END fn_get_prev_yr_wk;

   /***********************************************************************
   * Name: FN_GET_TBL_NM_SYN
   * Type: Function
   * Description: Get table name for given synonym
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        02/27/2014  Ankur Patel      1. Created this procedure.
   ************************************************************************/
   FUNCTION fn_get_tbl_nm_syn(p_syn_nm VARCHAR2)
      RETURN VARCHAR2 IS
      l_full_tbl_nm   VARCHAR2(30);
   BEGIN
      SELECT table_name
        INTO l_full_tbl_nm
        FROM user_synonyms
       WHERE synonym_name = p_syn_nm;

      RETURN l_full_tbl_nm;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in FN_GET_TBL_NM_SYN');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END fn_get_tbl_nm_syn;

   /***********************************************************************
   * Name: FN_GET_TBL_NM
   * Type: Function
   * Description: Get table name for given synonym and active flag
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        10/12/2011  Sandeep Manocha  1. Created this procedure.
   ************************************************************************/
   FUNCTION fn_get_tbl_nm(p_syn_nm        VARCHAR2,
                          p_tbl_actv      VARCHAR2)
      RETURN VARCHAR2 IS
      l_full_tbl_nm   VARCHAR2(30);
   BEGIN
      SELECT tbl_nm
        INTO l_full_tbl_nm
        FROM ctladm.syn_tbl_map
       WHERE synonym_nm = p_syn_nm
         AND actv_ind = p_tbl_actv;

      RETURN l_full_tbl_nm;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in FN_GET_TBL_NM');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
   END fn_get_tbl_nm;

   /***********************************************************************
   * Name: SET_SYN_TBL
   * Type: Procedure
   * Description: Set active flag for given table in synonym table.
   *  REVISIONS:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        10/12/2011  Sandeep Manocha  1. Created this procedure.
   ************************************************************************/
   PROCEDURE set_syn_tbl(p_tbl_nm        VARCHAR2,
                         p_tbl_actv      VARCHAR2) IS
   BEGIN
      UPDATE ctladm.syn_tbl_map
         SET actv_ind = p_tbl_actv, last_updt = SYSDATE
       WHERE tbl_nm = p_tbl_nm;

      IF (sql%ROWCOUNT = 0) THEN
         DBMS_OUTPUT.put_line('No rows to update in syn tbl map');
      END IF;
   EXCEPTION
      WHEN OTHERS THEN
         DBMS_OUTPUT.put_line('Error in SET_SYN_TBL');
         DBMS_OUTPUT.put_line(SUBSTR(SQLERRM, 1, 2000));
         RAISE;
   END set_syn_tbl;
END pkg_sls_pyr;
/




 