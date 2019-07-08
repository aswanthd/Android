CREATE OR REPLACE PACKAGE /*OV-SM-110118-01 */ SM_SEALDISCREPANCY
AS
    /*
       -----------------------------------------------------------------------------------------------------------------
       || Modification History
       || Version                  When           Who                       What
       || --------------------------------------------------------------------------------------------------------------
       || OV-SM-220818-01          22-AUG-2018    Vijayakanth.A         SM_SealDiscrepancy ( Seal Discrepancy.)
       || OV-SM-210918-01          21-SEP-2018    Yuvaraj K             Put procedure modified(merged hdr and dtl table).
	   || OV-SM-241218-01          24-DEC-2018    Praveen kumar. D      Consumer type,name, code added.
	   || OV-SM-110118-01          01-JAN-2019    vijayakanth.A         if seal range or single seal come box number should be entered to avoid mismatch between current stock and utilize seal.
	   || OV-SM-060319-01          06-MAR-2019    Sunilkumar Rajendran  Rel 1.6 Changes Impact in put_discrepancy_prc - BL Number, Container Number Commented	    	   
       -----------------------------------------------------------------------------------------------------------------
       */
    TYPE t_list IS REF CURSOR;
	
	PROCEDURE GetLov_smsud_PRC (
		p_token_id               IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
		p_input                  IN     Varchar2,--'B'
		P_sealbox_search         IN     VARCHAR2,--'X' -- Discrpancy - 'U' Utilization
		p_screen                 IN     VARCHAR2,
        P_sealtype               IN     VARCHAR2,
        P_seal_from		         IN     VARCHAR2, 
		P_seal_to	             IN     VARCHAR2,
        p_page_index             IN     NUMBER,
        p_page_size              IN     NUMBER,
        p_sort_field             IN     VARCHAR2,
        p_sort_type              IN     VARCHAR2,
		p_requestedby            IN     smsuh_sealutilizationheader.smbue_idorganization%TYPE,
		p_sealdiscrepancy_list          OUT sm_sealdiscrepancy.t_list,
		p_smsud_count           	   	OUT    number,
		p_issuccess                 OUT VARCHAR2,
		p_message                   OUT sm_sealdiscrepancy.t_list);
		
	/* Lov - List to show from and to seal number based on given boxnumber for full box selection */	
	PROCEDURE getlov_sealrange_prc (
        p_token_id               IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
        p_requestedby            IN     smsuh_sealutilizationheader.smbue_idorganization%TYPE,
		p_boxnumber              IN     smsud_sealutilizationdetail.smsud_boxnumber%TYPE,
        p_sealrange_list            OUT sm_sealdiscrepancy.t_list,
        p_issuccess                 OUT VARCHAR2,
        p_message                   OUT sm_sealdiscrepancy.t_list);

    /* GetList to show discrepancy box and sealnumbers */	
    PROCEDURE getlist_sealdiscrepancy_prc (
        p_token_id                IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
        p_requestedby             IN     smsuh_sealutilizationheader.smbue_idorganization%TYPE,
        p_smsud_sealnumber        IN     smsud_sealutilizationdetail.smsud_sealnumber%TYPE,
        p_smsud_boxnumber         IN     smsud_sealutilizationdetail.smsud_boxnumber%TYPE,
        p_smsud_createdby         IN     smsuh_sealutilizationheader.smsuh_createdby%TYPE,
        p_from_date               IN     smsuh_sealutilizationheader.smsuh_createdon%TYPE,
        p_to_date                 IN     smsuh_sealutilizationheader.smsuh_createdon%TYPE,
        p_smsud_status            IN     smsud_sealutilizationdetail.smsud_sealstatus%TYPE,
        p_page_index              IN     NUMBER,
        p_page_size               IN     NUMBER,
        p_sort_field              IN     VARCHAR2,
        p_sort_type               IN     VARCHAR2,
        p_sealdiscrepancy_list       OUT sm_sealdiscrepancy.t_list,
        p_sealdiscrepancy_count      OUT NUMBER,
        p_issuccess                  OUT VARCHAR2,
        p_message                    OUT sm_sealdiscrepancy.t_list);
		
     /* GetById to show discrepancy box and sealnumbers */	
    PROCEDURE get_sealdiscrepancy_prc (
        p_token_id                     IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
        p_smsuh_id                     IN     smsud_sealutilizationdetail.smsuh_id%TYPE,
        p_smbue_idrequestedby          IN     smsud_sealutilizationdetail.smbue_idorganization%TYPE,
        p_sealdiscrepancy_list            OUT sm_sealdiscrepancy.t_list,
        p_sealdiscrepancy_attachment      OUT sm_sealdiscrepancy.t_list,
        p_issuccess                       OUT VARCHAR2,
        p_message                         OUT sm_sealdiscrepancy.t_list);
		
    /* GetById to show attachment */	
    PROCEDURE get_smsua_prc (
        p_token_id                     IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
		p_smsua_id                     IN     smsua_sealutilizationattachment.smsua_id%TYPE,
        --p_smsuh_id                     IN     smsud_sealutilizationdetail.smsuh_id%TYPE,
        p_smbue_idrequestedby          IN     smsud_sealutilizationdetail.smbue_idorganization%TYPE,
        p_sealdiscrepancy_attachment      OUT sm_sealdiscrepancy.t_list,
        p_issuccess                       OUT VARCHAR2,
        p_message                         OUT sm_sealdiscrepancy.t_list);   
	
    /* Validation to check discrepancy, On clicking Save button */	
	PROCEDURE put_smsud_prc_validation (
        p_token_id                  IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,                
        p_smbue_idrequestedby       IN     smsud_sealutilizationdetail.smbue_idorganization%TYPE,        
		p_smsud_sealnumber          IN     smsud_sealutilizationdetail.smsud_sealnumber%TYPE,
        p_smsud_boxnumber           IN     smsud_sealutilizationdetail.smsud_boxnumber%TYPE,
        p_sealnumberfrom            IN     VARCHAR2,
        p_sealnumberto              IN     VARCHAR2,		
        p_stock_check                  OUT VARCHAR2,  --Y/N, Y -> Error     
        p_issuccess                    OUT VARCHAR2,
        p_message                      OUT sm_sealdiscrepancy.t_list);
    
 /* Save discrepancy, On clicking Save button - Discrepancy Edit mode -Update(U) / Cancel(C) / Discard(X) */	    
     PROCEDURE put_discrepancy_prc (
        p_token_id                  IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,        
        p_smsuh_id                  IN OUT smsud_sealutilizationdetail.smsuh_id%TYPE,
        p_smbue_idrequestedby       IN     smsud_sealutilizationdetail.smbue_idorganization%TYPE,
        p_smsud_id                  IN OUT smsud_sealutilizationdetail.smsud_id%TYPE,
        p_smsud_sealselectiontype   IN     smsud_sealutilizationdetail.smsud_sealselectiontype%TYPE,
		p_smsud_sealnumber          IN     smsud_sealutilizationdetail.smsud_sealnumber%TYPE,
        p_smsud_boxnumber           IN     smsud_sealutilizationdetail.smsud_boxnumber%TYPE,
        p_sealnumberfrom            IN     VARCHAR2,
        p_sealnumberto              IN     VARCHAR2,
		p_smsud_sealstatus          IN     smsud_sealutilizationdetail.smsud_sealstatus%TYPE,
		p_smsud_status              IN     smsud_sealutilizationdetail.smsud_status%TYPE,
		p_smsud_approvedby          IN     smsud_sealutilizationdetail.smsud_sealstatus%TYPE,
        p_smsud_remarks             IN     smsud_sealutilizationdetail.smsud_remarks%TYPE,
        p_user_id                   IN     smsud_sealutilizationdetail.smsud_createdby%TYPE,
        p_record_status             IN     VARCHAR2,--Insert (I) / Update(U) / Cancel(C) / Discard(X)
        p_issuccess                    OUT VARCHAR2,
        p_message                      OUT sm_sealdiscrepancy.t_list);

	     
	/* Upsert Seal utilization Attachment */ 
	PROCEDURE put_smsua_prc (
        p_token_id              IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
        p_smsua_id              IN OUT smsua_sealutilizationattachment.smsua_id%TYPE,
        p_smsuh_id              IN     smsua_sealutilizationattachment.smsuh_id%TYPE,
        p_smbue_idrequestedby   IN     smsua_sealutilizationattachment.smbue_idorganization%TYPE,
        p_smsua_filename        IN     smsua_sealutilizationattachment.smsua_filename%TYPE,
        p_smsua_fileextension   IN     smsua_sealutilizationattachment.smsua_fileextension%TYPE,
        p_smsua_filecontent     IN     smsua_sealutilizationattachment.smsua_filecontent%TYPE,
        p_user_id               IN     smsua_sealutilizationattachment.smsua_createdby%TYPE,
        p_record_status         IN     VARCHAR2,
        p_issuccess                OUT VARCHAR2,
        p_message                  OUT sm_sealdiscrepancy.t_list);
    
	/* Delete Seal utilization Attachment */
    PROCEDURE put_smsua_prc_del (
        p_token_id              IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
        p_smsua_id              IN     smsua_sealutilizationattachment.smsua_id%TYPE,
        p_smbue_idrequestedby   IN     smsua_sealutilizationattachment.smbue_idorganization%TYPE,
        p_issuccess                OUT VARCHAR2,
        p_message                  OUT sm_sealdiscrepancy.t_list);
	/* Procedure used to created by LOV in search list*/	
		  PROCEDURE Getlov_createdby_PRC (
        p_token_id         IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
        p_smbue_idorganization    IN     smusr_user.smbue_idorganization%TYPE,  
		p_from_date               IN     smsuh_sealutilizationheader.smsuh_createdon%TYPE,
		p_to_date                 IN     smsuh_sealutilizationheader.smsuh_createdon%TYPE,
		p_smsud_status            IN     smsud_sealutilizationdetail.smsud_sealstatus%TYPE,
        p_createdby_list      OUT sm_sealdiscrepancy.t_list,
        p_issuccess           OUT VARCHAR2,
        p_message             OUT sm_sealdiscrepancy.t_list);
END sm_sealdiscrepancy;
/
CREATE OR REPLACE PACKAGE BODY SM_SealDiscrepancy
AS

	PROCEDURE GetLov_smsud_PRC (
		p_token_id               IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
		p_input                  IN     Varchar2,
		P_sealbox_search         IN     VARCHAR2,
		p_screen                 IN     VARCHAR2,
        P_sealtype               IN     VARCHAR2,
        P_seal_from		         IN     VARCHAR2, 
		P_seal_to	             IN     VARCHAR2,
        p_page_index             IN     NUMBER,
        p_page_size              IN     NUMBER,
        p_sort_field             IN     VARCHAR2,
        p_sort_type              IN     VARCHAR2,
		p_requestedby            IN     smsuh_sealutilizationheader.smbue_idorganization%TYPE,
		p_sealdiscrepancy_list          OUT sm_sealdiscrepancy.t_list,
		p_smsud_count           	   	OUT    number,
		p_issuccess                 OUT VARCHAR2,
		p_message                   OUT sm_sealdiscrepancy.t_list)
	IS
	l_query         CLOB;
    l_query1        CLOB ;
		
    BEGIN  
	
	IF SM_SECURITY.SQL_INJECTION_CHECK_FNC (p_input) = 'Y'  OR
           SM_SECURITY.SQL_INJECTION_CHECK_FNC (P_sealbox_search) = 'Y' OR
		   SM_SECURITY.SQL_INJECTION_CHECK_FNC (p_screen) = 'Y'	        OR
		   SM_SECURITY.SQL_INJECTION_CHECK_FNC (P_sealtype) = 'Y'	    OR
		   SM_SECURITY.SQL_INJECTION_CHECK_FNC (P_seal_from) = 'Y'	    OR
		   SM_SECURITY.SQL_INJECTION_CHECK_FNC (P_seal_to) = 'Y'	    OR
		   SM_SECURITY.SQL_INJECTION_CHECK_FNC (p_sort_field) = 'Y'	    OR
		   SM_SECURITY.SQL_INJECTION_CHECK_FNC (p_sort_type) = 'Y'		   THEN  

	   l_query :='SELECT NULL  AS smsop_id,
				  NULL  AS smbue_id,
				  NULL  AS smsop_boxnumber,
				  NULL  AS smsop_sealnumber,
				  NULL  AS smsop_storename,
				  NULL  AS smsop_sealtype				 
			FROM  ovdul_dual 
		   where 1 = 2';  

	 p_smsud_count  := 0 ;            
	 OPEN p_sealdiscrepancy_list FOR l_query;
ELSE
  
    IF p_input='B' Then    
	 l_query :='
	   SELECT null as rno,
       smsop_id as smsop_id,
       smbue_id as smbue_id,
       smsop_boxnumber as smsop_boxnumber,
       null as smsop_sealnumber,
       smsop_storename as smsop_storename,
       smsop_sealtype as smsop_sealtype,
        min(smsop_sealnumber) as sealfrom, 
		max(smsop_sealnumber) as sealto 	   
FROM  (  
	   select distinct null AS smsop_id,
			   smsop.smbue_idorganization AS smbue_id,
			   smsop.SMSTK_BOXNUMBER AS smsop_boxnumber,			   
			   smsop.SMSTK_SEALNUMBER AS smsop_sealnumber,
			   null AS smsop_storename, 
               null as smsop_sealtype			   
		  from smstk_sealstock smsop                 		  
		  where ('''||p_requestedby|| ''' is null or smsop.smbue_idorganization =  '''||p_requestedby|| ''') 
          and   ('''||P_sealbox_search|| ''' is null or  smsop.SMSTK_BOXNUMBER  LIKE   ''%''|| '''|| P_sealbox_search || ''' || ''%'') 
		--  and   ('''||P_sealtype|| ''' is null or substr (smsop.SMSTK_SEALNUMBER,1,2) =  '''||P_sealtype|| ''') 
        --  and   ('''||P_seal_from|| ''' is null or smsop.SMSTK_SEALNUMBER >=  '''||P_seal_from|| ''')  
        --  and   ('''||P_seal_to|| ''' is null or smsop.SMSTK_SEALNUMBER <=  '''||P_seal_to|| ''')     
		   and smsop.SMSTK_BOXNUMBER is not null  and smsop.SMSTK_SEALSTATUS =''G''  
		   AND NOT EXISTS (SELECT ''X'' FROM SMSUD_SEALUTILIZATIONDETAIL SMSUD WHERE SMSOP.SMSTK_BOXNUMBER = SMSUD.SMSUD_BOXNUMBER AND  SMSUD.SMSUD_SEALSTATUS  in (''D'',''M'')
		                    and SMSUD.SMBUE_IDORGANIZATION=smsop.smbue_idorganization )
		    ) group by smsop_id,smbue_id,smsop_boxnumber,smsop_storename,smsop_sealtype  ' ;

	ELSE
	l_query :='
	 SELECT rownum as rno,
       smsop_id as smsop_id,
       smbue_id as smbue_id,
       smsop_boxnumber as smsop_boxnumber,
       smsop_sealnumber as smsop_sealnumber,
       smsop_storename as smsop_storename,
       smsop_sealtype as smsop_sealtype,
	    null as sealfrom, 
		null as sealto 
	  FROM  (select distinct null AS smsop_id,
			   smsop.smbue_idorganization AS smbue_id,
			   smsop.SMSTK_BOXNUMBER AS smsop_boxnumber,
			   smsop.SMSTK_SEALNUMBER AS smsop_sealnumber,
			   null AS smsop_storename, 
               null as smsop_sealtype			   
		  from smstk_sealstock smsop                 		  
		  where ('''||p_requestedby|| ''' is null or smsop.smbue_idorganization =  '''||p_requestedby|| ''') 
          and   ('''||P_sealbox_search|| ''' is null or smsop.SMSTK_SEALNUMBER LIKE  ''%''|| '''|| P_sealbox_search || ''' || ''%'') 
		  and   ('''||P_sealtype|| ''' is null or substr (smsop.SMSTK_SEALNUMBER,1,2) =  '''||P_sealtype|| ''') 
          and   ('''||P_seal_from|| ''' is null or smsop.SMSTK_SEALNUMBER  >=  '''||P_seal_from|| ''')  
          and   ('''||P_seal_to|| ''' is null or smsop.SMSTK_SEALNUMBER <=  '''||P_seal_to|| ''')  
		  and  smsop.SMSTK_SEALNUMBER is not null and smsop.SMSTK_SEALSTATUS =''G'') 		  ' ;   
			
    END IF;
	 
    /*
	 
		select distinct null AS smsop_id,
			   smsop.smbue_idorganization AS smbue_id,
			   smsop.smsop_boxnumber AS smsop_boxnumber,
			   null AS smsop_sealnumber,
			   null AS smsop_storename ,
               SMSOP_SEALTYPE as SMSOP_SEALTYPE			   
		  from smsop_sealopeningstock smsop                 		  
		  where  smsop.smbue_idorganization =  p_requestedby
		   and  NOT EXISTS ( select 'Y' FROM smsuh_sealutilizationheader smsuh join smsud_sealutilizationdetail smsud
		        on (smsuh.smsuh_id = smsud.smsuh_id and smsuh.smbue_idorganization = smsud.smbue_idorganization)
  		   where smsud.smsud_boxnumber =smsop.smsop_boxnumber
          and 	smsop.smbue_idorganization = smsud.smbue_idorganization
          and 	smsuh.smbue_idcustomer is not null)
		  and smsop.smsop_boxnumber is not null;   
    ELSE
     OPEN p_sealdiscrepancy_list FOR    
		select smsop.smsop_id AS smsop_id,
			   smsop.smbue_idorganization AS smbue_id,
			   smsop.smsop_boxnumber AS smsop_boxnumber,
			   smsop.smsop_sealnumber AS smsop_sealnumber,
			   smsop.smsop_storename AS smsop_storename  ,
               SMSOP_SEALTYPE as SMSOP_SEALTYPE				   
		  from smsop_sealopeningstock smsop
		  where  smsop.smbue_idorganization =  p_requestedby
		  	   and  NOT EXISTS ( select 'Y' FROM smsuh_sealutilizationheader smsuh join smsud_sealutilizationdetail smsud
		        on (smsuh.smsuh_id = smsud.smsuh_id and smsuh.smbue_idorganization = smsud.smbue_idorganization)
  		   where smsud.smsud_boxnumber =smsop.smsop_boxnumber   and smsud.smsud_sealnumber = smsop.smsop_sealnumber
          and 	smsop.smbue_idorganization = smsud.smbue_idorganization
          and 	smsuh.smbue_idcustomer is not null)		
          and smsop.smsop_boxnumber is not null and smsop.smsop_sealnumber is not null;
*/   

	l_query1 := 'SELECT COUNT(1) FROM (' || l_query || ') ';
			  
       EXECUTE IMMEDIATE l_query1 INTO p_smsud_count ;

            l_query := l_query || ' order by ';
            l_query := l_query || p_sort_field;
            l_query := l_query || ' ';
            l_query := l_query || p_sort_type;
            l_query := l_query|| ' offset ( ('|| p_page_index|| ' - 1) * '|| p_page_size|| ') rows fetch next '|| p_page_size|| ' rows only ';

	OPEN p_sealdiscrepancy_list FOR l_query;
	END IF;

      p_issuccess := sm_constants.k_success;
	  
    OPEN p_message FOR
        SELECT sm_constants.k_success_msgcode AS msg_code, NULL AS msg_value FROM OVDUL_Dual;
		
    EXCEPTION
			WHEN OTHERS THEN
				p_issuccess := sm_constants.k_failure;
				sm_exception.raise_exception;		
    END GetLov_smsud_PRC;
	
	  PROCEDURE Getlov_createdby_PRC (
        p_token_id         IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
        p_smbue_idorganization    IN     smusr_user.smbue_idorganization%TYPE,  
		p_from_date               IN     smsuh_sealutilizationheader.smsuh_createdon%TYPE,
		p_to_date                 IN     smsuh_sealutilizationheader.smsuh_createdon%TYPE,
		p_smsud_status            IN     smsud_sealutilizationdetail.smsud_sealstatus%TYPE,
        p_createdby_list      OUT sm_sealdiscrepancy.t_list,
        p_issuccess           OUT VARCHAR2,
        p_message             OUT sm_sealdiscrepancy.t_list)

    IS
    BEGIN
        OPEN p_createdby_list FOR
               SELECT DISTINCT smusr.smusr_id            AS smusr_id,
                      smusr.smusr_firstname || ' ' || smusr.smusr_lastname AS smusr_createdbyname,
				   CASE
					   WHEN TRUNC (NVL (smusr.smusr_deactivationdate, SYSDATE)) >=
							TRUNC (SYSDATE)
					   THEN
						   sm_constants.k_active
					   ELSE
						   sm_constants.k_inactive
				   END
					   AS smusr_status,
				   CASE
					   WHEN TRUNC (NVL (smusr.smusr_deactivationdate, SYSDATE)) >=
							TRUNC (SYSDATE)
					   THEN
						   sm_constants.k_activedesc
					   ELSE
						   sm_constants.k_inactivedesc
				   END
					   AS status_desc
FROM smsuh_sealutilizationheader smsuh  
join smsud_sealutilizationdetail smsud on (smsuh.smsuh_id=smsud.smsuh_id and smsuh.smbue_idorganization=smsud.smbue_idorganization)
join smusr_user smusr on (smusr.smusr_id = smsuh.smsuh_createdby )
WHERE smusr.smbue_idorganization = p_smbue_idorganization 
AND smsuh.smsuh_consumername IS NULL
AND smsud.smsud_sealstatus= p_smsud_status			
AND ( trunc (smsuh.smsuh_createdon) >= p_from_date)
AND ( trunc (smsuh.smsuh_createdon) <= p_to_date) ;

        p_issuccess := sm_constants.k_success;

        OPEN p_message FOR
            SELECT sm_constants.k_success_msgcode AS msg_code,
                   NULL                           AS msg_value
              FROM OVDUL_DUAL;
    EXCEPTION
        WHEN OTHERS
        THEN
            p_issuccess := sm_constants.k_failure;
            sm_exception.raise_exception;
    END;
	
	
	
	PROCEDURE getlov_sealrange_prc (
        p_token_id               IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
        p_requestedby            IN     smsuh_sealutilizationheader.smbue_idorganization%TYPE,
		p_boxnumber              IN     smsud_sealutilizationdetail.smsud_boxnumber%TYPE,
        p_sealrange_list            OUT sm_sealdiscrepancy.t_list,
        p_issuccess                 OUT VARCHAR2,
        p_message                   OUT sm_sealdiscrepancy.t_list)
	IS
    BEGIN      
     OPEN p_sealrange_list FOR   
	 select    null AS smsrh_id,
			   smsop.smbue_idorganization AS smbue_idorganization,
			   smsop.smstk_boxnumber AS smsrd_boxnumber, 
			   min(smstk_sealnumber) as sealnumberfrom, 
			   max(smstk_sealnumber) as sealnumberto	   
		  from smstk_sealstock smsop                 		  
		  where smsop.smbue_idorganization =  p_requestedby
		   and smsop.smstk_boxnumber 	   =  p_boxnumber
		   group by smsop.smbue_idorganization,smsop.smstk_boxnumber;
		   
		/*SELECT smsrd.smsrh_id             AS smsrh_id,
			   smsrd.smbue_idorganization AS smbue_idorganization,
			   smsrd.smsrd_boxnumber      AS smsrd_boxnumber,      
			   smsrd_sealnumberfrom       AS sealnumberfrom,
			   smsrd_sealnumberto         AS sealnumberto
		  FROM smsrd_sealreceiptdetail smsrd
		 WHERE     smbue_idorganization = p_requestedby
			   AND smsrd_boxnumber = p_boxnumber
			   and smsrd_sealnumberfrom is not null
		UNION
		SELECT null             AS smsrh_id,
			   smsrd.smbue_idorganization AS smbue_idorganization,
			   smsrd.smsrd_boxnumber      AS smsrd_boxnumber,      
			   min(smsrd_sealnumberfrom)  AS sealnumberfrom,
			   max(smsrd_sealnumberto)    AS sealnumberto
		  FROM smsrd_sealreceiptdetail smsrd
		 WHERE     smbue_idorganization = p_requestedby
			   AND smsrd_boxnumber = p_boxnumber
			   and smsrd_sealnumber is  not null
			   group by smsrd.smbue_idorganization,smsrd.smsrd_boxnumber
		UNION
		SELECT smsop.smsop_id             AS smsrh_id,
			   smsop.smbue_idorganization AS smbue_idorganization,
			   smsop.smsop_boxnumber      AS smsrd_boxnumber,      
			   smsop_sealnumberfrom       AS sealnumberfrom,
			   smsop_sealnumberto         AS sealnumberto
		  FROM smsop_sealopeningstock smsop
		 WHERE smbue_idorganization = p_requestedby AND smsop_boxnumber = p_boxnumber
		   and smsop_sealnumberfrom is not null
	   UNION
	   SELECT  null            AS smsrh_id,
			   smsop.smbue_idorganization AS smbue_idorganization,
			   smsop.smsop_boxnumber      AS smsrd_boxnumber,      
			   min(smsop_sealnumber)      AS sealnumberfrom,
			   max(smsop_sealnumber)         AS sealnumberto
		  FROM smsop_sealopeningstock smsop
		 WHERE smbue_idorganization = p_requestedby AND smsop_boxnumber = p_boxnumber
		 and smsop_sealnumber is  not null
		 group by smsop.smbue_idorganization,smsop.smsop_boxnumber;*/
    
      p_issuccess := sm_constants.k_success;
	  
    OPEN p_message FOR
        SELECT sm_constants.k_success_msgcode AS msg_code, NULL AS msg_value FROM OVDUL_Dual;
		
    EXCEPTION
			WHEN OTHERS THEN
				p_issuccess := sm_constants.k_failure;
				sm_exception.raise_exception;		
    END getlov_sealrange_prc;
 
	PROCEDURE getlist_sealdiscrepancy_prc (
		p_token_id                IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
		p_requestedby             IN     smsuh_sealutilizationheader.smbue_idorganization%TYPE,
		p_smsud_sealnumber        IN     smsud_sealutilizationdetail.smsud_sealnumber%TYPE,
		p_smsud_boxnumber         IN     smsud_sealutilizationdetail.smsud_boxnumber%TYPE,
		p_smsud_createdby         IN     smsuh_sealutilizationheader.smsuh_createdby%TYPE,
		p_from_date               IN     smsuh_sealutilizationheader.smsuh_createdon%TYPE,
		p_to_date                 IN     smsuh_sealutilizationheader.smsuh_createdon%TYPE,
		p_smsud_status            IN     smsud_sealutilizationdetail.smsud_sealstatus%TYPE,
		p_page_index              IN     NUMBER,
		p_page_size               IN     NUMBER,
		p_sort_field              IN     VARCHAR2,
		p_sort_type               IN     VARCHAR2,
		p_sealdiscrepancy_list       OUT sm_sealdiscrepancy.t_list,
		p_sealdiscrepancy_count      OUT NUMBER,
		p_issuccess                  OUT VARCHAR2,
		p_message                    OUT sm_sealdiscrepancy.t_list) 	
	IS
		l_query         CLOB;
        l_query1        CLOB ;
		

  
	BEGIN
 
	
	IF SM_SECURITY.SQL_INJECTION_CHECK_FNC (p_smsud_sealnumber) = 'Y' OR 
       SM_SECURITY.SQL_INJECTION_CHECK_FNC (p_smsud_boxnumber) = 'Y' OR 	 	
	   SM_SECURITY.SQL_INJECTION_CHECK_FNC (p_sort_field) = 'Y' OR
	   SM_SECURITY.SQL_INJECTION_CHECK_FNC (p_sort_type) = 'Y'  OR
	   SM_SECURITY.SQL_INJECTION_CHECK_FNC (p_smsud_status) = 'Y' 
	THEN   

	l_query :='SELECT 
                  null  as smsuh_id ,
                  null  as smsud_id, 
                  --null  as smbue_idcustomer  ,
                  null  as smbue_code ,
                  null  as smbue_name ,
                  null  as smsud_approvedby ,
                  null  as smsud_boxnumber ,
				  null  as smsud_sealnumber ,
                  null  as smsud_sealnumberfrom , --seal number from
                  null  as smsud_sealnumberto ,				  
                  null  as smsud_createdby  ,
                  null  as smsud_createdon,                  
                  null  as smsud_sealstatus ,
                  null  as seal_sealstatus_desc   
				  null  as 	manufacturedby
				FROM  ovdul_dual 
			   where 1 = 2';  

	     p_sealdiscrepancy_count  := 0 ;            
         OPEN p_sealdiscrepancy_list FOR l_query;
    ELSE


		l_query :='  SELECT smsuh.smsuh_id                          as smsuh_id ,
							smsud.smsud_id                          as smsud_id, 
							null              as smbue_idcustomer,
							smsuh.smsuh_consumertype                as smsuh_consumertype,
							smsuh.smsuh_consumercode                as smbue_code,
							smsuh.smsuh_consumername                as smbue_name,
                            smsud.smsud_approvedby                   AS smsud_approvedby, 
				            nvl(smsud.smsud_boxnumber,(select smseal.smstk_boxnumber from smstk_sealstock smseal where ((smseal.smstk_sealnumber=smsud.smsud_sealnumber) or (smseal.smstk_sealnumber between smsud.smsud_sealnumberfrom and smsud.smsud_sealnumberto) 
							or	(smsud.smsud_sealnumberfrom between smsud.smsud_sealnumber and  smseal.smstk_sealnumber)) and rownum=1)) as smsud_boxnumber ,
                            smsud.smsud_sealnumber                   AS smsud_sealnumber,
                            smsud.smsud_sealnumberfrom               AS smsud_sealnumberfrom,
                            smsud.smsud_sealnumberto                 AS smsud_sealnumberto,
                            smusr_firstname || '' '' || smusr_lastname AS smsud_createdby,
                            smsud.smsud_createdon                    as smsud_createdon,
                            smsud.smsud_sealstatus                   AS smsud_sealstatus,
                            smgec_st.smgec_description               AS seal_sealstatus_desc ,
							(select SMBUE_NAME 
							 from  smbue_businessentity smbuee 
                             join smbur_businessentityrole smbur on (smbur.smbue_id=smbuee.smbue_id and smbur.smbur_role=''M'')                               
                             join (SELECT smbue_id, smcnp_parametervalue
					                 FROM smcon_configuration  dscon
									JOIN smcnp_configurationparameters dscnp 
								    ON (dscon.smcon_id = dscnp.smcon_id AND smcon_referencename = ''MANUSEALTYPE'' AND smcnp_parametercode = ''MANUSEALTYPE'')) sealtype
								    ON (sealtype.smbue_id = smbuee.smbue_id  )
								   where smcnp_parametervalue = coalesce (substr(smsud.smsud_sealnumber,1,2),substr(smsud.smsud_sealnumberfrom,1,2),substr(smsud.smsud_sealnumberto,1,2)))
								   as   manufacturedby
                        from  smsuh_sealutilizationheader smsuh  
                        join smsud_sealutilizationdetail smsud on (smsuh.smsuh_id=smsud.smsuh_id and smsuh.smbue_idorganization=smsud.smbue_idorganization)				
                     
                        join smusr_user smusr on (smusr.smusr_id = smsud.smsud_createdby )
                        join smgec_generalcode smgec_st on (smgec_st.smgec_value = smsud.smsud_sealstatus and  smgec_st.smgec_referencename = ''SEALDISCREPANCYSTATUS'')
                         WHERE smsuh.smsuh_consumername is null
						    AND ('''||p_requestedby|| ''' IS NULL OR smsuh.smbue_idorganization='''||p_requestedby|| ''')
                            AND smsud.SMSUD_SEALSTATUS in (''M'',''D'') 
							AND smsud.smsud_status is null 
						    AND ('''||p_smsud_sealnumber|| ''' IS NULL OR  ( smsud.smsud_sealnumber = '''||p_smsud_sealnumber|| '''
					        OR (TO_NUMBER(regexp_replace('''||p_smsud_sealnumber|| ''',''[^0-9]'')) between TO_NUMBER(regexp_replace(smsud.smsud_sealnumberfrom,''[^0-9]''))  
   						                                                                        AND TO_NUMBER(regexp_replace(smsud.smsud_sealnumberto,''[^0-9]'')))))
									  AND ('''||p_smsud_boxnumber|| ''' IS NULL OR smsud.smsud_boxnumber='''||p_smsud_boxnumber|| ''')
									  AND ('''||p_smsud_createdby|| ''' IS NULL OR smsud.smsud_createdby='''||p_smsud_createdby|| ''')
									  AND ('''||p_smsud_status|| ''' IS NULL OR smsud.smsud_sealstatus='''||p_smsud_status|| ''')			
					        AND ( ( '''|| p_from_date|| ''' is null) or ( '''|| p_from_date|| ''' is not null and trunc (smsuh.smsuh_createdon) >= '''||  p_from_date|| '''))
				            AND ( ( '''|| p_to_date|| ''' is null)   or ( '''|| p_to_date|| ''' is not null and trunc (smsuh.smsuh_createdon) <= '''||  p_to_date|| ''')) ';

	l_query1 := 'SELECT COUNT(smsuh_id) FROM (' || l_query || ') ';
	
	
			  
       EXECUTE IMMEDIATE l_query1 INTO p_sealdiscrepancy_count ;

            l_query := l_query || ' order by ';
            l_query := l_query || p_sort_field;
            l_query := l_query || ' ';
            l_query := l_query || p_sort_type;
            l_query := l_query|| ' offset ( ('|| p_page_index|| ' - 1) * '|| p_page_size|| ') rows fetch next '|| p_page_size|| ' rows only ';

			
	OPEN p_sealdiscrepancy_list FOR l_query;
	
    END IF;

    p_issuccess := sm_constants.k_success;
    OPEN p_message FOR
        SELECT sm_constants.k_success_msgcode AS msg_code, NULL AS msg_value FROM OVDUL_Dual;		
		
	EXCEPTION
			WHEN OTHERS THEN
				p_issuccess := sm_constants.k_failure;
				sm_exception.raise_exception;			   
    END getlist_sealdiscrepancy_prc;				   

 
	PROCEDURE Get_sealdiscrepancy_PRC (
		p_token_id                     IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
		p_smsuh_id                     IN     smsud_sealutilizationdetail.smsuh_id%TYPE,
		p_smbue_idrequestedby          IN     smsud_sealutilizationdetail.smbue_idorganization%TYPE,
		p_sealdiscrepancy_list            OUT SM_SealDiscrepancy.t_list,
		p_sealdiscrepancy_attachment      OUT SM_SealDiscrepancy.t_list,
		p_issuccess                       OUT VARCHAR2,
		p_message                         OUT SM_SealDiscrepancy.t_list)
	IS  
	BEGIN
		OPEN  p_sealdiscrepancy_list    FOR 		   
			SELECT smsuh.smsuh_id                           AS smsuh_id,
                   smsuh.smbue_idorganization                AS smbue_idorganization,
                   smsud.smsud_id                           AS smsud_id,
                   smsud.smsud_sealselectiontype            AS smsud_sealselectiontype,
                   smgec.smgec_description                  AS sealselectiontype_desc,
                  -- smsuh.smbue_idcustomer                   AS smbue_idcustomer,
                   smsuh.smsuh_consumercode                 AS smbue_customercode,
				   smsuh.smsuh_consumertype                 AS smsuh_consumertype,
                   smsuh.smsuh_consumername                 AS smbue_customername,
                   smsud.smsud_approvedby                   AS smsud_approvedby,
                   smsud.smsud_boxnumber                    AS smsud_boxnumber,
                   smsud.smsud_sealnumber                   AS smsud_sealnumber,
                   smsud.smsud_sealnumberfrom               AS smsud_sealnumberfrom,
                   smsud.smsud_sealnumberto                 AS smsud_sealnumberto,
                   smusr_firstname || ' ' || smusr_lastname AS smsud_createdby,
                   smsud.smsud_sealstatus                   AS smsud_sealstatus,
                   smgec_st.smgec_description               AS seal_sealstatus_desc,
                   smsud.smsud_remarks                      AS smsud_remarks
              FROM smsuh_sealutilizationheader smsuh
				join smsud_sealutilizationdetail smsud on (smsuh.smsuh_id=smsud.smsuh_id and smsuh.smbue_idorganization=smsud.smbue_idorganization)
				--LEFT join smbue_businessentity smbue on (smbue.smbue_id=smsuh.smbue_idcustomer) 
				join smusr_user smusr on (smusr.smusr_id = smsud.smsud_createdby )
				join smgec_generalcode smgec on (smgec.smgec_value = smsud.smsud_sealselectiontype and  smgec.smgec_referencename = 'SEALSELECTIONTYPE')
				join smgec_generalcode smgec_st on (smgec_st.smgec_value = smsud.smsud_sealstatus and  smgec_st.smgec_referencename = 'SEALDISCREPANCYSTATUS')
				where smsuh.smsuh_id = p_smsuh_id 	  
				  and smsuh.smbue_idorganization = p_smbue_idrequestedby;	
				  
				  


				  
				  

		OPEN  p_sealdiscrepancy_attachment  FOR  
			SELECT smsua_id         as smsua_id,
				   smsuh_id         as smsuh_id,      
				   smsua_filename   as smsua_filename,
				   smsua_fileextension as smsua_fileextension
				  -- smsua_filecontent as smsua_filecontent
			  FROM smsua_sealutilizationattachment smsua
			where smsua.smsuh_id = p_smsuh_id  
			  and smsua.smbue_idorganization = p_smbue_idrequestedby;

	 p_issuccess := sm_constants.k_success;
	 
       OPEN p_message FOR
        SELECT sm_constants.k_success_msgcode AS msg_code, NULL AS msg_value FROM OVDUL_Dual;
	EXCEPTION
			WHEN OTHERS THEN
				p_issuccess := sm_constants.k_failure;
				sm_exception.raise_exception;	
	END Get_sealdiscrepancy_PRC;

	PROCEDURE Get_smsua_PRC (
		p_token_id                     IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
		p_smsua_id                     IN     smsua_sealutilizationattachment.smsua_id%TYPE,
		p_smbue_idrequestedby          IN     smsud_sealutilizationdetail.smbue_idorganization%TYPE,  
		p_sealdiscrepancy_attachment      OUT SM_SealDiscrepancy.t_list,
		p_issuccess                       OUT VARCHAR2,
		p_message                         OUT SM_SealDiscrepancy.t_list)		
	IS 	
	BEGIN
		 OPEN p_sealdiscrepancy_attachment FOR
		  SELECT smsua_id         as smsua_id,
			   smsuh_id         as smsuh_id,      
			   smsua_filename   as smsua_filename,
			   smsua_fileextension as smsua_fileextension,
			  smsua_filecontent as smsua_filecontent
		  FROM smsua_sealutilizationattachment smsua
		where smsua.smsua_id = p_smsua_id  
		  and smsua.smbue_idorganization = p_smbue_idrequestedby;
		  
		p_issuccess := sm_constants.k_success;

		OPEN p_message FOR
				SELECT sm_constants.k_success_msgcode AS msg_code,NULL AS msg_value FROM ovdul_dual;
    EXCEPTION
        WHEN OTHERS THEN
            p_issuccess := sm_constants.k_failure;
            sm_exception.raise_exception;	
	END Get_smsua_PRC;	
	
	PROCEDURE put_smsud_prc_validation (
        p_token_id                  IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,                
        p_smbue_idrequestedby       IN     smsud_sealutilizationdetail.smbue_idorganization%TYPE,        
		p_smsud_sealnumber          IN     smsud_sealutilizationdetail.smsud_sealnumber%TYPE,
        p_smsud_boxnumber           IN     smsud_sealutilizationdetail.smsud_boxnumber%TYPE,
        p_sealnumberfrom            IN     VARCHAR2,
        p_sealnumberto              IN     VARCHAR2,		
        p_stock_check                  OUT VARCHAR2, --Y/N      
        p_issuccess                    OUT VARCHAR2,
        p_message                      OUT sm_sealdiscrepancy.t_list)
	IS 	
		
	CURSOR box_validation IS
	SELECT count(*)
	  FROM smstk_sealstock smsop	     
	 WHERE smsop.smbue_idorganization = p_smbue_idrequestedby 	  
	   AND (p_smsud_boxnumber is null or smsop.smstk_boxnumber = p_smsud_boxnumber)
	   AND (smstk_sealnumber in (p_sealnumberfrom,p_sealnumberto)
	   OR  (smstk_sealnumber = p_sealnumberfrom  AND smstk_sealnumber = p_sealnumberto));
	  
	   
	CURSOR seal_validation IS
	SELECT count(*)
	  FROM smstk_sealstock smsop	     
	 WHERE smsop.smbue_idorganization = p_smbue_idrequestedby	   
	   AND (p_smsud_boxnumber is null or smsop.smstk_boxnumber = p_smsud_boxnumber) 
       AND (p_smsud_sealnumber is null or (smsop.smstk_sealnumber = p_smsud_sealnumber ))	
	   AND length(regexp_replace(p_smsud_sealnumber,'[0-9]')) = 2;
	  
	CURSOR seals_validation IS
	SELECT count(*)
	  FROM smstk_sealstock smsop	     
	 WHERE smsop.smbue_idorganization = p_smbue_idrequestedby	  
	   AND (p_smsud_boxnumber is null or smsop.smstk_boxnumber = p_smsud_boxnumber)
	   AND (smstk_sealnumber in (p_sealnumberfrom,p_sealnumberto)
	   OR  (smstk_sealnumber = p_sealnumberfrom  AND smstk_sealnumber = p_sealnumberto))
	   AND length(regexp_replace(p_sealnumberfrom,'[0-9]')) = 2 and length(regexp_replace(p_sealnumberto,'[0-9]')) = 2;
	   
	   
     
	
	l_stock_found number;
    l_seal_found number;	
    l_box_found  number;
    l_stock_check Varchar2(10);
	
	BEGIN
	l_stock_found :=null;
	l_box_found := null;
	l_seal_found := null;
	l_stock_check :='N';
	
	p_issuccess := sm_constants.k_success;

		OPEN p_message FOR
				SELECT sm_constants.k_success_msgcode AS msg_code,NULL AS msg_value FROM ovdul_dual;
				
	IF p_smsud_boxnumber IS NOT NULL THEN
	    OPEN box_validation;
		FETCH box_validation into l_box_found;
		CLOSE box_validation;
	
		IF l_box_found = 0 then
		--RAISE e_box_found;
		l_stock_check := 'Y';
		p_issuccess := sm_constants.k_failure;
		OPEN p_message FOR SELECT sm_message.SMSOP_CODE2 AS msg_code,sm_message.SMSOP_DESC2 AS msg_value FROM ovdul_dual;
	   END IF;	
   END IF;
   
	     IF p_smsud_sealnumber IS NOT NULL THEN
			OPEN seal_validation;
			FETCH seal_validation into l_seal_found;
			CLOSE seal_validation;
		
			IF l_seal_found= 0 then
			--RAISE e_seal_found;		
			l_stock_check := 'Y';
			p_issuccess := sm_constants.k_failure;
			OPEN p_message FOR SELECT sm_message.SMSOP_CODE3 AS msg_code,sm_message.SMSOP_DESC3 AS msg_value FROM ovdul_dual;
			END IF;
		ELSE
			OPEN seals_validation;
			FETCH seals_validation into l_stock_found;
			CLOSE seals_validation;
		
			IF l_stock_found = 0 then
			--RAISE e_stock_found;		
			l_stock_check := 'Y';
			p_issuccess := sm_constants.k_failure;
			OPEN p_message FOR SELECT sm_message.SMSOP_CODE4 AS msg_code,sm_message.SMSOP_DESC4 AS msg_value FROM ovdul_dual;
			END IF;
			END IF;		
		 
		p_stock_check :=l_stock_check;
		
    EXCEPTION
        WHEN OTHERS THEN
            p_issuccess := sm_constants.k_failure;
            sm_exception.raise_exception;	
	END put_smsud_prc_validation;	

    PROCEDURE put_discrepancy_prc (
        p_token_id                  IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,        
        p_smsuh_id                  IN OUT smsud_sealutilizationdetail.smsuh_id%TYPE,
        p_smbue_idrequestedby       IN     smsud_sealutilizationdetail.smbue_idorganization%TYPE,
        p_smsud_id                  IN OUT smsud_sealutilizationdetail.smsud_id%TYPE,
        p_smsud_sealselectiontype   IN     smsud_sealutilizationdetail.smsud_sealselectiontype%TYPE,
		p_smsud_sealnumber          IN     smsud_sealutilizationdetail.smsud_sealnumber%TYPE,
        p_smsud_boxnumber           IN     smsud_sealutilizationdetail.smsud_boxnumber%TYPE,
        p_sealnumberfrom            IN     VARCHAR2,
        p_sealnumberto              IN     VARCHAR2,
		p_smsud_sealstatus          IN     smsud_sealutilizationdetail.smsud_sealstatus%TYPE,
		p_smsud_status              IN     smsud_sealutilizationdetail.smsud_status%TYPE,
		p_smsud_approvedby          IN     smsud_sealutilizationdetail.smsud_sealstatus%TYPE,
        p_smsud_remarks             IN     smsud_sealutilizationdetail.smsud_remarks%TYPE, 		
        p_user_id                   IN     smsud_sealutilizationdetail.smsud_createdby%TYPE,
        p_record_status             IN     VARCHAR2,--Insert (I) / Update(U) / Cancel(C) / Discard(X)
        p_issuccess                    OUT VARCHAR2,
        p_message                      OUT sm_sealdiscrepancy.t_list)
	IS	
	e_box_found   exception;
    pragma exception_init (e_box_found, -20991);
	e_seal_found   exception;
    pragma exception_init (e_seal_found, -20992);
	e_stock_found   exception;
    pragma exception_init (e_stock_found, -20993);
	
	cursor exact_boxno
	is
	select smstk_boxnumber  from smstk_sealstock smstk  
	where (smstk_sealnumber = p_smsud_sealnumber or smstk_sealnumber = p_sealnumberfrom ) and p_smsud_sealselectiontype<>'F';
	
	
	 
	CURSOR box_validation IS
	SELECT count(*)
	  FROM smsud_sealutilizationdetail smsud	     
	 WHERE smsud.smbue_idorganization = p_smbue_idrequestedby 
	   AND smsud.SMSUD_SEALSTATUS in ('M','D') AND nvl (smsud.smsud_status,'$') not in ('C','X')
	   AND (p_smsud_boxnumber is null or smsud.smsud_boxnumber = p_smsud_boxnumber)
	   AND ((TO_NUMBER(regexp_replace(smsud_sealnumber,'[^0-9]')) between TO_NUMBER(regexp_replace(p_sealnumberfrom,'[^0-9]'))   AND TO_NUMBER(regexp_replace(p_sealnumberto,'[^0-9]')))
	   OR ((TO_NUMBER(regexp_replace(p_sealnumberfrom,'[^0-9]')) between TO_NUMBER(regexp_replace(smsud.smsud_sealnumberfrom,'[^0-9]'))   AND TO_NUMBER(regexp_replace(smsud.smsud_sealnumberto,'[^0-9]'))
	   OR TO_NUMBER(regexp_replace(p_sealnumberto,'[^0-9]')) between TO_NUMBER(regexp_replace(smsud.smsud_sealnumberfrom,'[^0-9]'))   AND TO_NUMBER(regexp_replace(smsud.smsud_sealnumberto,'[^0-9]')))));
	
	   
	CURSOR seal_validation IS
	SELECT count(*)
	  FROM smsud_sealutilizationdetail smsud	     
	 WHERE smsud.smbue_idorganization = p_smbue_idrequestedby
	   AND smsud.SMSUD_SEALSTATUS in ('M','D') AND nvl (smsud.smsud_status,'$') not in ('C','X')
	   AND (p_smsud_boxnumber is null or smsud.smsud_boxnumber = p_smsud_boxnumber) 
      AND ((p_smsud_sealnumber is null or (smsud_sealnumber = p_smsud_sealnumber
	   OR  (TO_NUMBER(regexp_replace(p_smsud_sealnumber,'[^0-9]')) between TO_NUMBER(regexp_replace(smsud.smsud_sealnumberfrom,'[^0-9]'))   AND TO_NUMBER(regexp_replace(smsud.smsud_sealnumberto,'[^0-9]'))))));

	   
	   
	CURSOR seals_validation IS
	SELECT count(*)
	  FROM smsud_sealutilizationdetail smsud	     
	 WHERE smsud.smbue_idorganization = p_smbue_idrequestedby
	   AND smsud.smsud_sealstatus in ('M','D') AND nvl (smsud.smsud_status,'$') not in ('C','X')
	   AND (p_smsud_boxnumber is null or smsud.smsud_boxnumber = p_smsud_boxnumber) 
       AND ((TO_NUMBER(regexp_replace(smsud_sealnumber,'[^0-9]')) between TO_NUMBER(regexp_replace(p_sealnumberfrom,'[^0-9]'))   AND TO_NUMBER(regexp_replace(p_sealnumberto,'[^0-9]')))
	   OR ((TO_NUMBER(regexp_replace(p_sealnumberfrom,'[^0-9]')) between TO_NUMBER(regexp_replace(smsud.smsud_sealnumberfrom,'[^0-9]'))   AND TO_NUMBER(regexp_replace(smsud.smsud_sealnumberto,'[^0-9]'))
	   OR TO_NUMBER(regexp_replace(p_sealnumberto,'[^0-9]')) between TO_NUMBER(regexp_replace(smsud.smsud_sealnumberfrom,'[^0-9]'))   AND TO_NUMBER(regexp_replace(smsud.smsud_sealnumberto,'[^0-9]')))));
		   
	--p_smsud_id number;	 
	l_stock_found number;
    l_seal_found number;	
    l_box_found  number;
    l_exact_box varchar2(50);	
	
	BEGIN
	l_stock_found :=null;
	l_box_found := null;
	l_seal_found := null;
	l_exact_box := null;
	
	IF p_record_status ='I' THEN 
	
	      OPEN exact_boxno ; 
		 FETCH exact_boxno into l_exact_box;
		 CLOSE exact_boxno;
		
		IF p_smsud_sealnumber IS NOT NULL AND p_smsud_sealselectiontype ='S' THEN
			OPEN seal_validation;
			FETCH seal_validation into l_seal_found;
			CLOSE seal_validation;
		
			IF l_seal_found > 0 then
			RAISE e_seal_found;		
			END IF;
		ELSIF p_smsud_sealselectiontype ='F' THEN --AND p_smsud_sealnumber IS NULL AND p_sealnumberfrom IS NULL AND p_sealnumberto IS NULL THEN
		
            OPEN box_validation;
            FETCH box_validation into l_box_found;
            CLOSE box_validation;
        
            IF l_box_found > 0 then
            RAISE e_box_found;		
            END IF;
		
	    ELSE
			OPEN seals_validation;
			FETCH seals_validation into l_stock_found;
			CLOSE seals_validation;
		
			IF l_stock_found > 0 then
			RAISE e_stock_found;		
			END IF;
		END IF;
		
	--p_smsud_id:=null;
	smsuh_sealutilizationheader_pkg.put_smsuh_prc (
				p_token_id               => p_token_id,      
				p_smsuh_id               => p_smsuh_id,      
			   -- p_smbue_idcustomer       => null,      
				p_smbue_idrequestedby    => p_smbue_idrequestedby,      
				p_smsuh_bookingnumber    => null ,      
				--p_smsuh_blnumber         => null ,      
				--p_smsuh_containernumber  => null ,      
				p_smsuh_otherreference   => null ,    
				p_smsuh_consumertype     => null , 
				p_smsuh_consumercode     => null , 
				p_smsuh_consumername     => null , 				
				p_user_id                => p_user_id,      
				p_record_status          => p_record_status,      
				p_issuccess              => p_issuccess,      
				p_message                => p_message);	
	 
	 IF p_smsud_sealselectiontype is not null and p_issuccess='S' THEN
			 SMSUD_SealUtilizationDetail_pkg.put_smsud_prc (
				p_token_id               => p_token_id,   
				p_smsud_id               => p_smsud_id,
				p_smsuh_id               => p_smsuh_id,
				p_smbue_idrequestedby    => p_smbue_idrequestedby,
				p_smsud_sealselectiontype=> p_smsud_sealselectiontype,
				p_smsud_boxnumber        => nvl (p_smsud_boxnumber,l_exact_box), 
				p_smsud_sealnumber       => p_smsud_sealnumber,
				p_smsud_sealnumberfrom   => p_sealnumberfrom,
		        p_smsud_sealnumberto     => p_sealnumberto,
				p_smsud_remarks          => p_smsud_remarks,
				p_smsud_sealstatus       => p_smsud_sealstatus,
			    p_smsud_status           => p_smsud_status,
				p_smsud_approvedby       => p_smsud_approvedby,
				p_user_id                => p_user_id,
				p_record_status          => p_record_status,
				p_issuccess              => p_issuccess, 
				p_message                => p_message);     
	
	 END IF;
	 
	 
				   
    ELSIF p_record_status = 'U' AND p_smsud_status = 'C' THEN
            UPDATE smsud_sealutilizationdetail
               SET smsud_remarks = smsud_remarks||' '||p_smsud_remarks,
                   smsud_status = p_smsud_status,
				   smsud_approvedby  = p_smsud_approvedby,
                   smsud_lastupdatedby = p_user_id,
				   
                   smsud_lastupdatedon = SYSDATE
             WHERE smsud_id = p_smsud_id and smbue_idorganization = p_smbue_idrequestedby;
   
     ELSIF p_record_status = 'U' AND p_smsud_status = 'X' THEN
              UPDATE smsud_sealutilizationdetail
               SET smsud_remarks = smsud_remarks||' '||p_smsud_remarks,
                   smsud_status = p_smsud_status,
				   smsud_approvedby  = p_smsud_approvedby,
                   smsud_lastupdatedby = p_user_id,
                   smsud_lastupdatedon = SYSDATE
             WHERE smsud_id = p_smsud_id and smbue_idorganization = p_smbue_idrequestedby;
     
            DELETE FROM  smsop_sealopeningstock  WHERE smbue_idorganization = p_smbue_idrequestedby
            AND (  (p_smsud_boxnumber IS NULL   OR p_smsud_boxnumber = smsop_boxnumber)
                  AND (p_smsud_sealnumber IS NULL OR p_smsud_sealnumber = smsop_sealnumber));

ELSIF   p_record_status = 'U'  THEN --p_record_status --> Update(U) / Cancel(C) / Discard(X) 
	       SMSUD_SealUtilizationDetail_pkg.put_smsud_prc (
				   p_token_id               => p_token_id,   
				   p_smsud_id               => p_smsud_id,
				   p_smsuh_id               => p_smsuh_id,
				   p_smbue_idrequestedby    => p_smbue_idrequestedby,
				   p_smsud_sealselectiontype=> p_smsud_sealselectiontype,
				   p_smsud_boxnumber        => p_smsud_boxnumber,
				   p_smsud_sealnumber       => p_smsud_sealnumber,
				   p_smsud_sealnumberfrom   => p_sealnumberfrom,
		           p_smsud_sealnumberto     => p_sealnumberto,
				   p_smsud_remarks          => p_smsud_remarks,
				   p_smsud_sealstatus       => p_smsud_sealstatus,
				   p_smsud_status           => p_smsud_status,
				   p_smsud_approvedby       => p_smsud_approvedby,
				   p_user_id                => p_user_id,
				   p_record_status          => p_record_status,
				   p_issuccess              => p_issuccess, 
				   p_message                => p_message);  			 
			    
			 
						   
     END IF;
	EXCEPTION
	    WHEN e_box_found THEN	
			p_issuccess := sm_constants.k_failure;
			open p_message for select sm_message.smsnd_code4 as msg_code,sm_message.smsnd_desc4 as msg_value from ovdul_dual;
		WHEN e_seal_found THEN	
			p_issuccess := sm_constants.k_failure;
			open p_message for select sm_message.smsnd_code3 as msg_code,sm_message.smsnd_desc3 as msg_value from ovdul_dual;
	    WHEN e_stock_found THEN	
			p_issuccess := sm_constants.k_failure;
			open p_message for select sm_message.smsnd_code5 as msg_code,sm_message.smsnd_desc5 as msg_value from ovdul_dual;
		WHEN OTHERS THEN
				p_issuccess := sm_constants.k_failure;
				sm_exception.raise_exception;	
	END put_discrepancy_prc;
	
	
	
	 
	
	PROCEDURE put_smsua_prc (
		p_token_id              IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
		p_smsua_id              IN OUT smsua_sealutilizationattachment.smsua_id%TYPE,
		p_smsuh_id              IN     smsua_sealutilizationattachment.smsuh_id%TYPE,
		p_smbue_idrequestedby   IN     smsua_sealutilizationattachment.smbue_idorganization%TYPE,
		p_smsua_filename        IN     smsua_sealutilizationattachment.smsua_filename%TYPE,
		p_smsua_fileextension   IN     smsua_sealutilizationattachment.smsua_fileextension%TYPE,
		p_smsua_filecontent     IN     smsua_sealutilizationattachment.smsua_filecontent%TYPE,
		p_user_id               IN     smsua_sealutilizationattachment.smsua_createdby%TYPE,
		p_record_status         IN     VARCHAR2,
		p_issuccess                OUT VARCHAR2,
		p_message                  OUT sm_sealdiscrepancy.t_list)
	IS
	BEGIN
		smsua_sealutilizationattach_pkg.put_smsua_prc (
			p_token_id              => p_token_id,
			p_smsua_id              => p_smsua_id,
			p_smsuh_id              => p_smsuh_id,
			p_smbue_idrequestedby   => p_smbue_idrequestedby,
			p_smsua_filename        => p_smsua_filename,
			p_smsua_fileextension   => p_smsua_fileextension,
			p_smsua_filecontent     => p_smsua_filecontent,
			p_user_id               => p_user_id,
			p_record_status         => p_record_status,
			p_issuccess             => p_issuccess,
			p_message               => p_message);
	EXCEPTION 	
	WHEN OTHERS THEN
			p_issuccess := sm_constants.k_failure;
			sm_exception.raise_exception;
	END put_smsua_prc;

	PROCEDURE put_smsua_prc_del (
		p_token_id              IN     smlsc_loginsessioncontrol.smlsc_tokenid%TYPE,
		p_smsua_id              IN     smsua_sealutilizationattachment.smsua_id%TYPE,
		p_smbue_idrequestedby   IN     smsua_sealutilizationattachment.smbue_idorganization%TYPE,
		p_issuccess                OUT VARCHAR2,
		p_message                  OUT sm_sealdiscrepancy.t_list)
	IS
	BEGIN
		smsua_sealutilizationattach_pkg.put_smsua_prc_del (
			p_token_id              => p_token_id,
			p_smsua_id              => p_smsua_id,
			p_smbue_idrequestedby   => p_smbue_idrequestedby,
			p_issuccess             => p_issuccess,
			p_message               => p_message);
		
	EXCEPTION
		WHEN OTHERS
		THEN
			p_issuccess := sm_constants.k_failure;
			sm_exception.raise_exception;
	END put_smsua_prc_del;     
				   
END SM_SealDiscrepancy;
/