SCHEMA mydb
DEFINE cust_arr DYNAMIC ARRAY OF RECORD
     store_num    LIKE customer.store_num,
     store_name   LIKE customer.store_name,
     addr         LIKE customer.addr,
     addr2        LIKE customer.addr2,
     city         LIKE customer.city,
     state        LIKE customer.state,
     zipcode      LIKE customer.zipcode,
     contact_name LIKE customer.contact_name,
     phone        LIKE customer.phone
    END RECORD

--MAIN
--  DEFINE idx SMALLINT
-- 
--   DEFER INTERRUPT
--   CONNECT TO "mydb"
--   CLOSE WINDOW SCREEN
--   OPEN WINDOW w3 WITH FORM "table"
--
--   CALL load_custall() RETURNING idx
--
--   IF idx > 1 THEN
--      CALL inpupd_custall(idx)
--   END IF
--  
--   CLOSE WINDOW w3
--   DISCONNECT CURRENT
--
--END MAIN

-----------------------------------------------------
FUNCTION load_custall()
-----------------------------------------------------
  DEFINE idx     SMALLINT

  DECLARE custlist_curs CURSOR FOR
   SELECT store_num, 
          store_name, 
          addr,
          addr2,
          city, 
          state, 
          zipcode, 
          contact_name, 
          phone
        FROM customer
        ORDER BY store_num
 
  LET idx = 1
  WHENEVER ERROR CONTINUE
  FOREACH custlist_curs INTO cust_arr[idx].*
     LET idx = idx + 1
  END FOREACH
  WHENEVER ERROR STOP
    
  IF (idx > 1) THEN 
    CALL cust_arr.deleteElement(idx)
    LET idx = idx -1 
  ELSE
    MESSAGE "No rows loaded." 
  END IF   

  RETURN idx

END FUNCTION
---------------------------------------------------------------

FUNCTION inpupd_custall(idx)#insert
-------------------------------------------------------------
  DEFINE idx            SMALLINT,
         curr_pa        SMALLINT,
         opflag         CHAR(1)

 
  LET opflag = "U"

  INPUT ARRAY cust_arr WITHOUT DEFAULTS 
     FROM sa_cust.* 
     ATTRIBUTE (UNBUFFERED, COUNT=idx)
      

  BEFORE INPUT 
    MESSAGE "OK to exit/" ||
    "Cancel to exit & cancel current operation"

  BEFORE ROW
    LET curr_pa = ARR_CURR()

  BEFORE INSERT
    LET opflag = "I" 
    
  AFTER INSERT
   CALL insert_cust(curr_pa)  
 
  BEFORE DELETE
    IF NOT (delete_cust(curr_pa)) THEN
      CANCEL DELETE
    END IF
      
  ON ROW CHANGE
    IF opflag = "U" THEN
      CALL update_cust(curr_pa) 
    ELSE
      LET opflag = "U" 
    END IF
    
  BEFORE FIELD store_num
    IF (opflag  <> "I") THEN
      NEXT FIELD store_name
    END IF
 
  ON CHANGE store_num
    IF opflag = "I" THEN
      IF NOT store_num_ok(curr_pa) THEN
        MESSAGE "Store already exists"
        LET cust_arr[curr_pa].store_num = NULL
        NEXT FIELD store_num
      END IF 
    END IF 
     
     
  END INPUT
  
  IF (int_flag) THEN
    LET int_flag = FALSE
  END IF
 
END FUNCTION
 
-----------------------------------------------------

FUNCTION store_num_ok (idx)
------------------------------------------
  DEFINE idx SMALLINT,
         checknum  LIKE customer.store_num,
         cont_ok   SMALLINT

  LET cont_ok = FALSE
  WHENEVER ERROR CONTINUE 
  SELECT store_num 
     INTO checknum
     FROM customer 
     WHERE store_num = cust_arr[idx].store_num
  WHENEVER ERROR STOP     
  IF (SQLCA.SQLCODE = NOTFOUND) THEN
    LET cont_ok = TRUE 
  ELSE
    LET cont_ok = FALSE
    IF (SQLCA.SQLCODE = 0) THEN 
       ERROR "Store Number already exists."
    ELSE
       ERROR SQLERRMESSAGE
    END IF
  END IF

  RETURN cont_ok

END FUNCTION

----------------------------------------------------------
FUNCTION insert_cust(idx)
----------------------------------------------------------
  DEFINE idx SMALLINT
  
  WHENEVER ERROR CONTINUE
  INSERT INTO customer
        (store_num, 
         store_name, 
         addr, 
         addr2, 
         city, 
         state, 
         zipcode, 
         contact_name, 
         phone)
    VALUES (cust_arr[idx].* )
   WHENEVER ERROR STOP
  
   IF (SQLCA.SQLCODE = 0) THEN
     MESSAGE "Dealer added"
   ELSE
     ERROR SQLERRMESSAGE
   END IF
  
END FUNCTION

---------------------------------------------------------------
FUNCTION update_cust(idx)
---------------------------------------------------------------
  DEFINE idx SMALLINT

  WHENEVER ERROR CONTINUE
  UPDATE customer SET 
     store_name = cust_arr[idx].store_name,
     addr = cust_arr[idx].addr,
     addr2 = cust_arr[idx].addr2, 
     city = cust_arr[idx].city, 
     state = cust_arr[idx].state, 
     zipcode = cust_arr[idx].zipcode, 
     contact_name = cust_arr[idx].contact_name, 
     phone = cust_arr[idx].phone
     WHERE store_num =  cust_arr[idx].store_num
   WHENEVER ERROR STOP
 
   IF (SQLCA.SQLCODE = 0) THEN
     MESSAGE "Dealer updated."
   ELSE
     ERROR SQLERRMESSAGE
   END IF

END FUNCTION

-------------------------------------------------------------
FUNCTION delete_cust(idx)
-------------------------------------------------------------
  DEFINE idx     SMALLINT,
         del_ok  SMALLINT
      
      --MENU "Delete" ATTRIBUTE 
      --     (STYLE="dialog", 
      --      COMMENT="Delete this row?")
      --COMMAND "OK"
      --  LET del_ok = TRUE
      --  EXIT MENU
      --COMMAND "Cancel"
      --  LET del_ok = FALSE 
      --  EXIT MENU
      --END MENU
      
      IF del_ok = TRUE THEN 
        WHENEVER ERROR CONTINUE
        DELETE FROM customer WHERE store_num = 
           cust_arr[idx].store_num
        WHENEVER ERROR STOP
        IF (SQLCA.SQLCODE = 0) THEN
          LET del_ok = TRUE
          MESSAGE "Dealer deleted."
        ELSE
          LET del_ok = FALSE
          ERROR SQLERRMESSAGE
        END IF
      END IF
 
     RETURN del_ok
  
END FUNCTION

----------------------------------------------------------

