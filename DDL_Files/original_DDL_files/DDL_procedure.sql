


  CREATE OR REPLACE PROCEDURE "XDMADM"."PROC_XDM_ORD_GD_MAINT" IS
   /***********************************************************************
   * Name: FN_ORD_GD_MAINT
   * Type: Function
   * Description: Validate DDMAMD.ORD_GD_CORP.  Gather Stats. Truncate oldest partition
   * Revisions:
   *  Ver        Date        Author           Description
   *  ---------  ----------  ---------------  -----------------------------
   *  1.0        10/28/2013  Steve Chang       Created this function
   ************************************************************************/
   c_success                   CONSTANT NUMBER := 0;
   c_error                     CONSTANT NUMBER := 1;
   c_schema                    CONSTANT VARCHAR2(30) := 'XDMADM';
   c_tbl_nm           		   CONSTANT VARCHAR2(30) := 'ORD_GD_CORP';
   c_load_tbl_nm               CONSTANT VARCHAR2(30) := 'ORD_GD_CORP';

   l_nbr_wks				       NUMBER;
   l_maint_wks            NUMBER;
   l_return_code               NUMBER;
   l_out_msg                   VARCHAR2(256);
   l_out_msg2                  VARCHAR2(256);
   l_number_of_wks			   NUMBER;
   l_partition_name			   VARCHAR2(100);

BEGIN

	--Call Gather Statistics
        l_return_code := xdmadm.fn_getstat_tbl(c_schema, c_load_tbl_nm);
    DBMS_OUTPUT.put_line('Analyze Table Return: ' || l_return_code || '-' || l_out_msg);

  IF (l_return_code <> 0) THEN
      DBMS_OUTPUT.put_line('Error: Failed to analyze table');
    --  RETURN SQLCODE;
  Else
       DBMS_OUTPUT.put_line('Table Analyzed');
  End If;

  --grab number of weeks to maintain from partition table
  SELECT PARAM_VALUE_NBR
  INTO l_maint_wks
  FROM XDMADM.PARAM_VALUE
  WHERE APP_NAME = 'PROC_ORD_GD_MAINT';

	--Call Validate Data -check for 5 week of data
	  SELECT COUNT(DISTINCT WKLY_RUN_DT)
  	INTO l_nbr_wks
  	FROM XDMADM.ORD_GD_CORP;

    --Call to drop partition.  Find oldest partition first.
	IF l_nbr_wks > l_maint_wks THEN
		--DROP PARTITION
		Select Obj.Subobject_Name
		Into L_Partition_Name
		From All_Objects Obj
        , all_tab_partitions part
		WHERE obj.object_type = 'TABLE PARTITION'
		And Obj.Object_Name = 'ORD_GD_CORP'
    And Obj.Owner = 'XDMADM'
    and Obj.object_name = part.table_name
    And Obj.Owner = Part.Table_Owner
    and Obj.Subobject_Name = Part.Partition_Name
    And Part.Num_Rows>0
    and obj.subobject_name<>'ORD_GD_CORP_WK1'
		And Obj.Timestamp = (Select Min(Obj2.Timestamp)
                  From All_Objects Obj2
                  , all_tab_partitions part2
                  WHERE obj2.object_type = obj.object_type
                  And Obj2.Object_Name = Obj.Object_Name
                  And Obj2.Object_Name = Part2.Table_Name
                  and Obj2.Subobject_Name = Part2.Partition_Name
                  And Obj2.Owner = Part2.Table_Owner
                  and Part2.num_rows>0
                  And Obj2.Owner = 'XDMADM'
                  and obj2.subobject_name<>'ORD_GD_CORP_WK1');

		l_return_code   :=
         usfdba.table_pkg.drop_partition(l_out_msg,
                                             c_schema,
                                             c_load_tbl_nm,
                                             l_partition_name);

	  Dbms_Output.Put_Line('Drop Partition Return: ' || L_Return_Code );
    Dbms_Output.Put_Line('outmssage ' || L_Out_Msg);

      IF (l_return_code <> 0) THEN
         DBMS_OUTPUT.put_line('Error: Failed to drop partition');
       --  RETURN 1;
      END IF;
  ELSE
      DBMS_OUTPUT.put_line('No Partition to Drop');
	END IF;

	--Call Gather Statistics
        l_return_code := xdmadm.fn_getstat_tbl(c_schema, c_load_tbl_nm);
    DBMS_OUTPUT.put_line('Analyze Table Return: ' || l_return_code || '-' || l_out_msg);

  IF (l_return_code <> 0) THEN
      DBMS_OUTPUT.put_line('Error: Failed to analyze table');
    --  RETURN SQLCODE;
  ELSE
       DBMS_OUTPUT.put_line('Table Analyzed');
  END IF;

  --RETURN 0;

EXCEPTION
   WHEN OTHERS THEN
      DBMS_OUTPUT.put_line('Unexepected error');
      DBMS_OUTPUT.put_line(SQLCODE || ': ' || SQLERRM);
   -- RETURN SQLCODE;
END; --PROC_DDM_ORD_GD_MAINT
/




  CREATE OR REPLACE PROCEDURE "XDMADM"."TRUNC_TBL" (
   p_trgt_schm        in varchar2 -- the owner the table which will be truncated
   ,p_trgt_tbl        in varchar2 -- the table which will be truncated
   ,p_trgt_part       in varchar2 default null -- if used will only truncate the named partition
   ,p_trgt_type       in varchar2 default null -- if used will say if a partition or subpartition
) is
 v_trgt_schm     varchar2(30) := upper(p_trgt_schm);
 v_trgt_tbl      varchar2(30) := upper(p_trgt_tbl);
 v_trgt_part     varchar2(30) := upper(p_trgt_part);
 stmt            varchar2(32767);
begin
 if v_trgt_part is null then
  stmt := 'truncate table ' || v_trgt_schm || '.' || v_trgt_tbl;
  dbms_output.put_line(stmt);
  execute immediate stmt;
 else
  stmt := 'alter table ' || v_trgt_schm || '.' || v_trgt_tbl || ' truncate ' || p_trgt_type || '(' || v_trgt_part || ')';
  dbms_output.put_line(stmt);
  execute immediate stmt;
 end if;
end trunc_tbl;
/




  CREATE OR REPLACE PROCEDURE "XDMADM"."INCR_LOAD_STAT" (p_fact_table IN VARCHAR2,
                          p_div_nbr IN NUMBER,
                          p_fact_typ_cd IN VARCHAR2,
                          p_odate IN DATE) AS

/***********************************************************************
** Program Name:  XDMADM.INCR_LOAD_STAT
**
** Description: Incremental Procedure for xdmadm.fact_load_stat
**
**
** Input parameter:
**
**
** Output parameters :
**
**
**
** Error Handling:
**
** Modification Log
**  Sept 9 2011, Create new  procedure to increment fact_load_stat entires to +1.
**
**
************************************************************************/
   c_enable_execute    CONSTANT INTEGER := 1; -- 1 to enable
   c_enable_commit     CONSTANT INTEGER := 1; -- 1 to enable

     l_corp_update_sql        VARCHAR2(8000);
     l_ap_update_sql          VARCHAR2(8000);
     l_reg_update_sql         VARCHAR2(2000);
		 l_trans_update_sql       VARCHAR2(2000);
     l_trans_typ              VARCHAR2(2);
     l_fact_typ_cd            VARCHAR2(2);
     l_fact_table             VARCHAR2(100);
     l_div_nbr                NUMBER;
     l_odate                  DATE;
     maxPrcsDt                DATE;

BEGIN
l_fact_typ_cd := p_fact_typ_cd;
l_fact_table := p_fact_table;
l_div_nbr := p_div_nbr;

dbms_output.put_line('Start');

--If div_nbr is 0 then update record.  Use for all corporate level records
IF l_div_nbr = 0 THEN

        DBMS_OUTPUT.put_line('Start Corporate Update for '||l_fact_table);
                  UPDATE   xdmadm.fact_load_stat
                     SET   div_load_ts   = sysdate,
                           div_load_stat = div_load_stat + 1,
                           latest_prcs_Dt = p_odate
                   WHERE   fact_table = l_fact_table
										 and   latest_prcs_Dt <= p_odate;

                DBMS_OUTPUT.put_line('Updated ' || sql%ROWCOUNT || ' rows for ' || l_fact_table);
END IF;
-- Update ANY table other than SALES CORP AND OPO CORP and division is not zero
IF l_fact_table not in ('SALES_CORP', 'PO_CORP') and l_div_nbr != 0  THEN
    DBMS_OUTPUT.put_line ('Start '||l_fact_table||' Update');
     l_reg_update_sql  := 'update xdmadm.fact_load_stat
                   set  div_load_stat = div_load_stat + 1,
                        div_load_ts = sysdate,
                        latest_prcs_dt = '''||trunc(p_odate)||'''
								 where  latest_prcs_dt <=  '''||trunc(p_odate)||'''
								  and   fact_table =''' ||l_fact_table||'''
									and   (div_nbr is null or div_nbr ='''|| l_div_nbr||''')
								  and   (fact_typ_cd is null or fact_typ_cd =''' ||l_fact_typ_cd||''')';
    DBMS_OUTPUT.put_line(l_reg_update_sql);
    EXECUTE IMMEDIATE  l_reg_update_sql;
        DBMS_OUTPUT.put_line('Updated ' || sql%ROWCOUNT || ' rows for ' || p_fact_table);


-- UPDATE REG SALES RECORDS.  Find the max prcs date from the stgadm.sales_xxx_ext table
ELSIF l_fact_table = 'SALES_CORP'  AND l_fact_typ_cd = 'RG' THEN
DBMS_OUTPUT.put_line ('Start SALES_CORP RG Update');

EXECUTE IMMEDIATE
       'select max(prcs_dt)
          from stgadm.sales_'||p_div_nbr||'_ext where trans_typ != ''AP'' and xfer_from_dt is null 'into maxPrcsDt ;
                    dbms_output.put_line('Prcs_Dt for stgadm.sales_'||p_div_nbr||'_ext:'||maxPrcsDt);

                    l_trans_update_sql    :=
 'update xdmadm.fact_load_stat
  set    div_load_stat = div_load_stat +1,
                 div_load_ts = sysdate,
         latest_prcs_dt = '''||maxPrcsDt||'''
  where  latest_prcs_dt <= '''||maxPrcsDt||'''
  and    fact_table =''' ||l_fact_table||'''
  and    div_nbr =' ||l_div_nbr || '
  and    fact_typ_cd =''' ||l_fact_typ_cd||'''';

                    dbms_output.put_line(l_trans_update_sql);

           EXECUTE IMMEDIATE l_trans_update_sql;
           DBMS_OUTPUT.put_line('Updated ' || sql%ROWCOUNT || ' REG SALES_CORP rows in FACT_LOAD_STAT');

--Update PO records that have a trans type of PR RC and RA.  That converts to RG in the fact_load_stat Table
ELSIF l_fact_table = 'PO_CORP'  AND l_fact_typ_cd = 'RG'  THEN
DBMS_OUTPUT.put_line ('Start PO_CORP RG Update');
EXECUTE IMMEDIATE
       'select max(prcs_dt)
          from stgadm.PO_'||p_div_nbr||'_ext where trans_typ not in (''SB'', ''SS'') 'into maxPrcsDt ;
             dbms_output.put_line('Prcs_Dt for stgadm.PO_'||p_div_nbr||'_ext:'||maxPrcsDt);

                    l_trans_update_sql    :=
 'update xdmadm.fact_load_stat
  set    div_load_stat = div_load_stat +1,
                 div_load_ts = sysdate,
         latest_prcs_dt = '''||maxPrcsDt||'''
  where  latest_prcs_dt <= '''||maxPrcsDt||'''
  and    fact_table =''' ||l_fact_table||'''
  and    div_nbr =' ||l_div_nbr || '
  and    fact_typ_cd =''' ||l_fact_typ_cd||'''';
           dbms_output.put_line(l_trans_update_sql);

      EXECUTE IMMEDIATE l_trans_update_sql;
     DBMS_OUTPUT.put_line('Updated ' || sql%ROWCOUNT || ' REG PO_CORP rows in FACT_LOAD_STAT');

--Update sales records with trans typ of AP.
ELSIF  l_fact_table = 'SALES_CORP' AND l_fact_typ_cd = 'AP' THEN
DBMS_OUTPUT.put_line ('Start SALES CORP AP Update');
EXECUTE IMMEDIATE
       'select max(prcs_dt)
          from stgadm.sales_'||p_div_nbr||'_ext where trans_typ = ''AP'' and xfer_from_dt is null 'into maxPrcsDt ;
     dbms_output.put_line('Prcs_Dt for stgadm.sales_'||p_div_nbr||'_ext:'||maxPrcsDt);

                          l_trans_update_sql    :=
 'update xdmadm.fact_load_stat
  set    div_load_stat = div_load_stat +1,
                 div_load_ts = sysdate,
         latest_prcs_dt = '''||maxPrcsDt||'''
  where  latest_prcs_dt <= '''||maxPrcsDt||'''
  and    fact_table =''' ||l_fact_table||'''
  and    div_nbr =' ||l_div_nbr || '
  and    fact_typ_cd =''' ||l_fact_typ_cd||'''';
            dbms_output.put_line(l_trans_update_sql);

           EXECUTE IMMEDIATE l_trans_update_sql;
           DBMS_OUTPUT.put_line('Updated ' || sql%ROWCOUNT || ' AP SALES_CORP rows in FACT_LOAD_STAT');
--Update PO_CORP with trans typ of SB and SS
ELSIF l_fact_table = 'PO_CORP'  AND l_fact_typ_cd = 'SB' THEN
DBMS_OUTPUT.put_line ('Start PO_CORP SB Update');
EXECUTE IMMEDIATE
       'select max(prcs_dt)
          from stgadm.PO_'||p_div_nbr||'_ext where trans_typ not in (''PR'', ''RC'', ''RA'') 'into maxPrcsDt ;
     dbms_output.put_line('Prcs_Dt for stgadm.sales_'||p_div_nbr||'_ext:'||maxPrcsDt);

                           l_trans_update_sql    :=
 'update xdmadm.fact_load_stat
  set    div_load_stat = div_load_stat +1,
                 div_load_ts = sysdate,
         latest_prcs_dt = '''||maxPrcsDt||'''
  where  latest_prcs_dt <= '''||maxPrcsDt||'''
  and    fact_table =''' ||l_fact_table||'''
  and    div_nbr =' ||l_div_nbr || '
  and    fact_typ_cd =''' ||l_fact_typ_cd||'''';
            dbms_output.put_line(l_trans_update_sql);

           EXECUTE IMMEDIATE l_trans_update_sql;

           DBMS_OUTPUT.put_line('Updated ' || sql%ROWCOUNT || ' SB PO_CORP rows in FACT_LOAD_STAT');
            END IF;

   IF (c_enable_commit = 1) THEN
      COMMIT;
            ELSE ROLLBACK;
   END IF;

   DBMS_OUTPUT.put_line ('End Process');

EXCEPTION
    WHEN NO_DATA_FOUND
      THEN NULL;
   WHEN OTHERS THEN
      ROLLBACK;
      DBMS_OUTPUT.put_line(SQLERRM || ':' || SQLERRM);
END incr_load_stat;
/




  CREATE OR REPLACE PROCEDURE "XDMADM"."RESET_LOAD_STAT" (p_fact_table IN Varchar2,
                               p_fact_typ_cd IN Varchar,
                               p_fact_table_dsply_nm IN Varchar2,
                               p_fact_typ_desc IN Varchar2,
                               p_load_freq IN Varchar2,
                               p_rpt_dsply_ind IN Varchar2,
                               p_odate IN Date)
       AS
/***********************************************************************
** Program Name:  XDMADM.RESET_LOAD_STAT
**
** Description: Exadata End Of Day Procedure
**
**
** Input parameter:
**
**
** Output parameters :
**
**
**
** Error Handling:
**
** Modification Log
**  Sept 9 2011, Create new EOD procedure to reset fact_load_stat entires to 0.
**
**
************************************************************************/


latestPrcsdt            DATE;
l_opo_div               NUMBER;
l_fact_typ_cd           Varchar(2 Byte);
l_fact_table            VARCHAR2(100);
l_fact_table_dsply_nm   VARCHAR2(100);
l_fact_typ_desc         VARCHAR2(100);
l_load_freq             VARCHAR2(100);
l_div_nbr               number;
l_rpt_dsply_ind         VARCHAR2(2 Byte);
l_day_of_week           Varchar2(100);

  TYPE cur_type IS REF CURSOR;
  div_cur  cur_type;
    TYPE rec_ty IS RECORD
  ( div_nbr NUMBER);

  div_rec rec_ty;

BEGIN

dbms_output.put_line('Start');

dbms_output.put_line('Delete inactive records');
            DELETE FROM xdmadm.fact_load_stat
            WHERE div_nbr NOT IN (SELECT  dc.div_nbr
                                 FROM   xdmadm.div_corp dc
                                  WHERE   (dc.conv_to_div_nbr IS NULL
																		OR    dc.conv_on_date > SYSDATE)
																	 AND    dc.inact_dt IS NULL
                                    AND   dc.eiw_actv > 0)
                                    AND div_nbr != 0
                                    AND fact_table = p_fact_table;
                DBMS_OUTPUT.put_line('DELETED ' || sql%ROWCOUNT || ' rows for ' || p_fact_table);

  OPEN div_cur FOR
            SELECT   dc.div_nbr
            FROM   xdmadm.div_corp dc
           WHERE   (dc.conv_to_div_nbr IS NULL
                    OR dc.conv_on_date > SYSDATE)
                    AND   dc.inact_dt IS NULL
                    AND   dc.eiw_actv > 0;

LOOP
  FETCH div_cur INTO div_rec;
  EXIT WHEN div_cur%NOTFOUND;

            l_fact_typ_cd := p_fact_typ_cd;
            l_div_nbr := div_Rec.div_nbr;
            l_fact_table := p_fact_table;
            l_fact_table_dsply_nm:= p_fact_table_dsply_nm ;
            l_fact_typ_desc := p_fact_typ_desc;
            l_load_freq:= p_load_freq;
						l_rpt_dsply_ind:= p_rpt_dsply_ind;
      dbms_output.put_line(l_fact_table ||' '|| l_div_nbr ||' ' || l_fact_typ_cd);

        --Insert new record for division that does not exist for fact_table, div_nbr and fact_typ_cd

  INSERT INTO xdmadm.fact_load_stat
      (SELECT l_fact_table,l_div_nbr, l_fact_typ_cd, l_fact_table_dsply_nm, l_fact_typ_desc, TRUNC(p_odate), 1, (SYSDATE), TRUNC(p_odate),l_load_freq, l_rpt_dsply_ind
         FROM div_corp div
         WHERE div.div_nbr = l_div_nbr
        AND NOT EXISTS (SELECT 1
                            FROM xdmadm.fact_load_stat org
                           WHERE (org.div_nbr =l_div_nbr or div_nbr = 0)
                            AND   org.fact_table= l_fact_table
	                          AND  (org.fact_typ_cd is null or org.fact_typ_cd = l_fact_typ_cd))) ;

            DBMS_OUTPUT.put_line('Inserted ' || sql%ROWCOUNT || ' rows for ' || l_fact_table);
    END LOOP;
CLOSE div_cur;

    /* Find latest prcs dt to compare against odate.  Do not update to 0 if the prcs dt is > odate*/

    SELECT   max(latest_prcs_dt)
     INTO    latestPrcsdt
     FROM    xdmadm.fact_load_stat
     WHERE   fact_table = l_fact_table
     AND     (fact_typ_cd is null or fact_typ_cd = l_fact_typ_cd);
         DBMS_OUTPUT.put_line('Latest Prcs Dt '|| latestPrcsdt || ' ' || l_fact_table || ' ' || p_odate || ' ' || l_day_of_week);

     SELECT trim(TO_CHAR(trunc(p_odate), 'DAY')) INTO l_day_of_week FROM dual;
     DBMS_OUTPUT.put_line('Day of Week '|| l_day_of_week);

     IF latestPrcsdt <= p_odate  AND (l_fact_typ_cd is null or l_fact_typ_cd not in ('AP', 'SB', 'SA')) THEN
      dbms_output.put_line('Start Daily Update');

         UPDATE xdmadm.fact_load_stat
          SET div_load_stat = 0,
              div_load_ts = SYSDATE,
              prcs_dt = p_odate +1
        WHERE  fact_table = l_fact_table
          AND (fact_typ_cd is null or fact_typ_cd = l_fact_typ_cd);
    DBMS_OUTPUT.put_line('Updated' ||' '|| sql%ROWCOUNT || ' rows for ' || l_fact_table || ' ' || l_fact_typ_cd || ' ' || l_day_of_week);

             ELSIF (l_fact_typ_cd in ('AP', 'SB', 'SA') and l_day_of_week ='SATURDAY') THEN
             dbms_output.put_line('Start Weekly Update');

						  UPDATE xdmadm.fact_load_stat
                 SET div_load_stat = 0,
                     div_load_ts = SYSDATE,
                     prcs_dt = p_odate +1
               WHERE fact_table = l_fact_table
								AND (fact_typ_cd is null or fact_typ_cd = l_fact_typ_cd);
                 DBMS_OUTPUT.put_line('Updated' ||' '|| sql%ROWCOUNT || ' rows for ' || l_fact_table || ' ' || l_fact_typ_cd );
              END IF;
COMMIT;
     DBMS_OUTPUT.put_line ('End Process');

EXCEPTION
    WHEN NO_DATA_FOUND
      THEN NULL;
   WHEN OTHERS THEN
      ROLLBACK;
      DBMS_OUTPUT.put_line(SQLERRM || ':' || SQLERRM);
END reset_load_stat;
/




  CREATE OR REPLACE PROCEDURE "XDMADM"."FACT_LOAD_STAT_BAKUP" ( p_odate IN Date)
   AS
/***********************************************************************
** Program Name:  XDMADM.FACT_LOAD_STAT_BAKUP
**
** Description: xdmadm.fact_load_stat_bak procedure that populates fact_load_stat_bak.
                This table is a snapshot of fact_load_stat.  It runs once a day
**
** Modification Log
**  Oct 24 2011. Created Procedure
**
**
************************************************************************/


bakTs   CONSTANT DATE := SYSDATE;

BEGIN

dbms_output.put_line('Start');
dbms_output.put_line('Insert into FACT_LOAD_STAT_BAK');

Insert into XDMADM.FACT_LOAD_STAT_BAK
(select bakTs, fact_table, div_nbr, fact_typ_cd, fact_table_dsply_nm, fact_typ_desc, latest_prcs_dt, div_load_stat, div_load_ts, prcs_dt, load_freq, rpt_dsply_ind from fact_load_stat);

DBMS_OUTPUT.put_line('INSERTED ' || sql%ROWCOUNT || ' rows into FACT_LOAD_STAT_BAK');

     DBMS_OUTPUT.put_line ('End Process');
COMMIT;

EXCEPTION
    WHEN NO_DATA_FOUND
      THEN NULL;
   WHEN OTHERS THEN
      ROLLBACK;
      DBMS_OUTPUT.put_line(SQLERRM || ':' || SQLERRM);

END FACT_LOAD_STAT_BAKUP;
/






  CREATE OR REPLACE PROCEDURE "XDMADM"."COMPRESS_SUBPARTITION"
(
p_schm   varchar2,
p_tbl    varchar2,
p_end_dt date
)
AS
  l_highval     VARCHAR2(4000);
  i_highval     VARCHAR2(4000);
  l_subhighval  VARCHAR2(4000);
  i_subhighval  VARCHAR2(4000);
  v_div_nbr     number;
  v_tgt_tab     varchar2(32);
  v_end_dt      date;
  v_stmt1       varchar2(2000);
  v_stmt2       varchar2(2000);
  --Compression varabile
  v_sls_compr_intvl NUMBER;
  v_skip_sls_compr_intvl NUMBER;
BEGIN
  v_div_nbr := substr(p_tbl,instr(p_tbl,'_')+1);
  dbms_output.put_line('Div Nbr: '|| v_div_nbr);
  v_tgt_tab := substr(p_tbl,1,instr(p_tbl,'_'))||'CORP';
  dbms_output.put_line('TGT Tab: '|| v_tgt_tab);

--******************************************************************************************
--* Retrieve skip sales interval to know how many days back from sysdate to SKIP compression
--* due to still recieving sales data updates for those days such as PA's
--******************************************************************************************

     <<RETRIEVE_SKIP_SLS_COMPR_INTRVL>>
      BEGIN

            SELECT param_value_nbr
              INTO v_skip_sls_compr_intvl
              FROM xdmadm.param_value
             WHERE app_name = 'COMPRESSION'
               AND param_type = 'COMPR_INTVL'
               AND param_code = 'SKIP_SLS_COMPR_INTVL';

             dbms_output.put_line('Skip Sales Compression Interval from param value is: ' || v_skip_sls_compr_intvl);

             EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                      DBMS_OUTPUT.PUT_LINE ('No Data Found in PARAM_VALUE where app_name = COMPRESSION and param_type = COMPR_INTVL and param_code = SKIP_SLS_COMPR_INTVL');
                      DBMS_OUTPUT.PUT_LINE ('Exception Encountered: '||SQLCODE||' - '|| SQLERRM);
                      RAISE;
                  WHEN OTHERS THEN
                      DBMS_OUTPUT.PUT_LINE('Exception Encountered: '||SQLCODE||' - '|| SQLERRM);
                      RAISE;
      END;

--******************************************************************************
--* Set v_end_dt. If null, set p_end_dt to know how far back to start compress
--******************************************************************************

  IF p_end_dt IS NULL THEN
      BEGIN
            /*
            ===============================================================================
            * Retrieve the sales interval days to see how far back from sysdate to compress
            ===============================================================================
            */
            SELECT param_value_nbr
              INTO v_sls_compr_intvl
              FROM xdmadm.param_value
             WHERE app_name = 'COMPRESSION'
               AND param_type = 'COMPR_INTVL'
               AND param_code = 'SLS_COMPR_INTVL';

             dbms_output.put_line('Sales Compression Interval from param value is: ' || v_sls_compr_intvl);

             EXCEPTION
                  WHEN NO_DATA_FOUND THEN
                      DBMS_OUTPUT.PUT_LINE ('No Data Found in PARAM_VALUE where app_name = COMPRESSION and param_type = COMPR_INTVL and param_code = SLS_COMPR_INTVL');
                      DBMS_OUTPUT.PUT_LINE ('Exception Encountered: '||SQLCODE||' - '|| SQLERRM);
                      RAISE;
                  WHEN OTHERS THEN
                      DBMS_OUTPUT.PUT_LINE('Exception Encountered: '||SQLCODE||' - '|| SQLERRM);
                      RAISE;
      END;
    --Commented out to now take parameter from PARAM_VALUE
    --v_end_dt := sysdate-14;
      v_end_dt := sysdate-v_sls_compr_intvl;
  ELSE
    v_end_dt := p_end_dt;
  END IF;
  dbms_output.put_line ('End Dt: '||v_end_dt);


  FOR c1 IN (SELECT partition_name
                   ,high_value
               FROM user_tab_partitions
              WHERE table_name = v_tgt_tab
              ORDER BY partition_position DESC)
  LOOP
    /*
    ============================================================================
    * Convert the high_value from LONG to VARCHAR2
    ============================================================================
    */

    l_highval := c1.high_value;
    /*
    ============================================================================
    * Get the Year-Month-Day value from the High_Value
    ============================================================================
    */
    l_highval := substr(l_highval, 11, 11);
    --dbms_output.put_line('High Water Mark for Partition '||c1.partition_name|| ': ' ||  l_highval);


 --   IF to_date(l_highval,'yyyy-mm-dd') < SYSDATE-14 THEN
    IF to_date(l_highval,'yyyy-mm-dd') < SYSDATE-v_skip_sls_compr_intvl THEN
        dbms_output.put_line('Partition: '||c1.partition_name|| ' HV: '||l_highval);
        FOR c2 IN (SELECT subpartition_name,tablespace_name, compress_for
                         ,high_value
                     FROM user_tab_subpartitions
                    WHERE table_name = v_tgt_tab
                      AND partition_name = c1.partition_name
                    ORDER BY subpartition_position)
        LOOP
            l_subhighval := c2.high_value;

            IF l_subhighval = to_char(v_div_nbr) THEN
                dbms_output.put_line('Subpartition: '||c2.subpartition_name|| ' HV: '||l_subhighval||' FOR: '||c2.compress_for);
                v_stmt1 := 'ALTER TABLE '||p_schm||'.'||v_tgt_tab||' MOVE SUBPARTITION '||c2.subpartition_name|| ' TABLESPACE '||c2.tablespace_name||' COMPRESS FOR '||c2.compress_for;
                dbms_output.put_line(v_stmt1);
                BEGIN
                  execute immediate (v_stmt1);
                  EXCEPTION
                    WHEN OTHERS THEN
                      dbms_output.put_line('sql: '||v_stmt1);
                      dbms_output.put_line('Exception Encountered: '||SQLCODE||' - '|| SQLERRM);
                  RAISE;
                END;
                EXIT;
            END IF;
        END LOOP;
      IF to_date(l_highval,'yyyy-mm-dd') < v_end_dt THEN
        EXIT;
      END IF;
    END IF;
  END LOOP;

  FOR i0 in (SELECT index_name
               FROM user_indexes
              WHERE table_owner = p_schm AND table_name = v_tgt_tab)
  LOOP
    FOR i1 IN (SELECT partition_name
                     ,high_value
                 FROM user_ind_partitions
                WHERE index_name = i0.index_name
                ORDER BY partition_position DESC)
    LOOP
      /*
      ============================================================================
      * Convert the high_value from LONG to VARCHAR2
      ============================================================================
      */
      i_highval := i1.high_value;
      /*
      ============================================================================
      * Get the Year-Month-Day value from the High_Value
      ============================================================================
      */
      i_highval := substr(i_highval, 11, 11);

    --  IF to_date(i_highval,'yyyy-mm-dd') < SYSDATE-14 THEN
        IF to_date(i_highval,'yyyy-mm-dd') < SYSDATE-v_skip_sls_compr_intvl THEN
          dbms_output.put_line('Partition: '||i1.partition_name|| ' HV: '||i_highval);
          FOR i2 IN (SELECT subpartition_name,tablespace_name
                           ,high_value
                       FROM user_ind_subpartitions
                      WHERE index_name = i0.index_name
                        AND partition_name = i1.partition_name
                      ORDER BY subpartition_position)
          LOOP
              i_subhighval := i2.high_value;

              IF i_subhighval = to_char(v_div_nbr) THEN
                dbms_output.put_line('Subpartition: '||i2.subpartition_name|| ' HV: '||i_subhighval);
                v_stmt2 := 'ALTER INDEX '||p_schm||'.'||i0.index_name||' REBUILD SUBPARTITION '||i2.subpartition_name|| ' TABLESPACE '||i2.tablespace_name;
                dbms_output.put_line(v_stmt2);
                BEGIN
                  execute immediate (v_stmt2);
                  EXCEPTION
                    WHEN OTHERS THEN
                      dbms_output.put_line('sql: '||v_stmt1);
                      dbms_output.put_line('Exception Encountered: '||SQLCODE||' - '|| SQLERRM);
                  RAISE;
                END;
                EXIT;
              END IF;
          END LOOP;
        IF to_date(i_highval,'yyyy-mm-dd') < v_end_dt THEN
          EXIT;
        END IF;
      END IF;
    END LOOP;
  END LOOP;
end compress_subpartition;
/



