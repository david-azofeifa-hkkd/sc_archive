


  CREATE OR REPLACE TRIGGER "XDMADM"."DROP_CORP_ORIG_LDR_TM_T"
  BEFORE INSERT ON XDMADM.DROP_CORP     FOR EACH ROW
BEGIN
        :NEW.ORIG_LDR_TM := :NEW.LDR_TM;
      END;
/
ALTER TRIGGER "XDMADM"."DROP_CORP_ORIG_LDR_TM_T" ENABLE;




  CREATE OR REPLACE TRIGGER "XDMADM"."TRG_U_I_CUST_CHNSUMM_CORP"

BEFORE UPDATE OR INSERT ON XDMADM.CUST_CHNSUMM_CORP
     FOR EACH ROW
BEGIN
          IF :NEW.ldr_tm is NOT NULL and :old.ldr_tm <> :new.ldr_tm then
            null;
          else
            :NEW.ldr_tm := to_number(to_char( sysdate , 'yyyymmddhh24miss'));
          end if;
      END;
/
ALTER TRIGGER "XDMADM"."TRG_U_I_CUST_CHNSUMM_CORP" ENABLE;


