IMPORT util
IMPORT FGL comboboxHelper
IMPORT FGL DynamicUI
IMPORT FGL UIHelper
SCHEMA mydb

CONSTANT cBaseSQL = "SELECT * FROM test"
CONSTANT cOrderBy = "id"
DEFINE m_customer RECORD LIKE test.*
DEFINE rec RECORD 
      --store_num LIKE customer.store_num,
      store_name LIKE customer.store_name,
      phone LIKE customer.phone 
      END RECORD 
 

DEFINE fetchAction RECORD
    firstRecord INTEGER,
    previousRecord INTEGER,
    nextRecord INTEGER,
    lastRecord INTEGER
END RECORD = (
    firstRecord: 0,
    previousRecord: 1,
    nextRecord: 2,
    lastRecord: 3
)

DEFINE inputMode RECORD
    addMode INTEGER,
    changeMode INTEGER
END RECORD = (
    addMode: 0,
    changeMode: 1
)


MAIN 
   
    DATABASE mydb

    OPEN WINDOW inputForm WITH FORM "input_dialog"
    CLOSE WINDOW SCREEN
    CALL ui.Window.getCurrent().getForm().loadToolBar("RecordManager")
          
    WHILE searchCustomer()
        DISPLAY "Looping back to search"
    END WHILE
    
    CLOSE WINDOW inputForm
      
END MAIN

PRIVATE FUNCTION searchCustomer() RETURNS BOOLEAN
    DEFINE lWherePart STRING
    DEFINE r_customer RECORD LIKE test.*
    DEFINE lReturnStatus BOOLEAN = FALSE
    
    DISPLAY "Search Mode" TO formonly.mode_label
    LET lWherePart = DynamicUI.dynamicConstruct(base.TypeInfo.create(r_customer))
    IF lWherePart.getLength() > 0 THEN
        IF buildQuery(lWherePart) THEN
            CALL viewCustomer()
            CALL closeCustomerCursor()
            LET lReturnStatus = TRUE
        ELSE
            ERROR "No matches found"
        END IF
    END IF

    RETURN lReturnStatus
    
END FUNCTION

PRIVATE FUNCTION buildQuery(pWherePart STRING) RETURNS BOOLEAN
    DEFINE lSQLSelect STRING
    DEFINE lReturnStatus BOOLEAN = TRUE

    IF pWherePart MATCHES " WHERE*" THEN
        LET lSQLSelect = cBaseSQL, pWherePart
    ELSE
        LET lSQLSelect = SFMT("%1\n WHERE %2 ORDER BY %3", cBaseSQL, pWherePart, cOrderBy)
    END IF
    TRY
        DECLARE cursCustomers SCROLL CURSOR FROM lSQLSelect
        OPEN cursCustomers
        FETCH FIRST cursCustomers INTO m_customer.*
        IF SQLCA.SQLCODE > 0 THEN
            LET lReturnStatus = FALSE
            CALL closeCustomerCursor()
        END IF
    CATCH
        LET lReturnStatus = FALSE
        CALL closeCustomerCursor()
    END TRY

    RETURN lReturnStatus

END FUNCTION #buildQuery

PRIVATE FUNCTION fetchRecord(pFetchAction INTEGER) RETURNS BOOLEAN
    DEFINE lReturnStatus BOOLEAN = FALSE

    TRY
        CASE pFetchAction
            WHEN fetchAction.firstRecord
                FETCH FIRST cursCustomers INTO m_customer.*
            WHEN fetchAction.previousRecord
                FETCH PREVIOUS cursCustomers INTO m_customer.*
            WHEN fetchAction.nextRecord
                FETCH NEXT cursCustomers INTO m_customer.*
            WHEN fetchAction.lastRecord
                FETCH LAST cursCustomers INTO m_customer.*
            OTHERWISE
                RETURN lReturnStatus
        END CASE
        LET lReturnStatus = (SQLCA.sqlcode == 0)
    CATCH
        LET lReturnStatus = FALSE
    END TRY

    RETURN lReturnStatus

END FUNCTION #fetchRecord

PRIVATE FUNCTION closeCustomerCursor() RETURNS ()

    TRY
        CLOSE cursCustomers
    CATCH
        #Ignore errors on close cursor
    END TRY

END FUNCTION #closeCustomerCursor

PRIVATE FUNCTION viewCustomer() RETURNS ()

    LET int_flag = FALSE
    WHILE int_flag == FALSE

        DISPLAY m_customer.* TO s_customer.*
        DISPLAY "View Mode" TO formonly.mode_label
        
        MENU
            ON ACTION CANCEL
                LET int_flag = TRUE
                EXIT MENU
            ON ACTION FIRST
                IF fetchRecord(fetchAction.firstRecord) THEN
                    EXIT MENU
                END IF
            ON ACTION PREVIOUS
                IF  (fetchAction.previousRecord) THEN
                    EXIT MENU
                END IF
            ON ACTION NEXT
                IF fetchRecord(fetchAction.nextRecord) THEN
                    EXIT MENU
                END IF
            ON ACTION LAST
                IF fetchRecord(fetchAction.lastRecord) THEN
                    EXIT MENU
                END IF
            ON ACTION add_rec
                IF inputCustomer(inputMode.addMode) THEN
                    #Need to re-create cursor???
                    EXIT MENU
                END IF
            ON ACTION edit_rec
                IF inputCustomer(inputMode.changeMode) THEN
                    #Need to re-create cursor???
                    EXIT MENU
                END IF
            ON ACTION del_rec
                IF UIHelper.deleteConfirmation() THEN
                    #Should delete here, but I won't
                    IF fetchRecord(fetchAction.previousRecord) THEN
                        EXIT MENU
                    END IF
                END IF
        END MENU

    END WHILE
    LET int_flag  = FALSE

END FUNCTION #viewCustomer

PUBLIC FUNCTION inputCustomer(pMode INTEGER) RETURNS BOOLEAN
    DEFINE lReturnStatus BOOLEAN = FALSE
    DEFINE r_customer RECORD LIKE test.*
    DEFINE lStoreNum LIKE test.id

    #Do Input Statement...
    IF pMode == inputMode.changeMode THEN
        LET r_customer.* = m_customer.*
        DISPLAY "Change mode" TO formonly.mode_label
    ELSE
        LET r_customer.id = 0
        DISPLAY "Add mode" TO formonly.mode_label
    END IF


    INPUT r_customer.* WITHOUT DEFAULTS FROM s_customer.*
     ATTRIBUTES(UNBUFFERED, ACCEPT=FALSE)
        BEFORE INPUT
            IF pMode == inputMode.changeMode THEN
                CALL DIALOG.setFieldActive("s_customer.id", FALSE)
            END IF
        AFTER FIELD id
            IF pMode == inputMode.addMode AND (r_customer.id) > 0 THEN
                SELECT id INTO lStoreNum
                  FROM test WHERE id = r_customer.id
                IF SQLCA.SQLCODE == 0 AND LENGTH(lStoreNum) > 0 THEN
                    #Store number record already exists
                    ERROR "id already exists in the database"
                    NEXT FIELD id
                END IF
            END IF
        
        ON ACTION save
            ACCEPT INPUT
        ON ACTION CANCEL
            LET int_flag = TRUE
            EXIT INPUT
        ON CHANGE state
            DISPLAY SFMT("State value changed to %1", r_customer.state)
        AFTER INPUT
            IF LENGTH(r_customer.id) == 0 THEN
                ERROR "Store name is missing"
                NEXT FIELD fname
            END IF
    END INPUT

    IF int_flag THEN
        LET lReturnStatus = FALSE
    ELSE
        TRY
            IF pMode == inputMode.addMode THEN
                INSERT INTO test VALUES(r_customer.*)
            ELSE
                UPDATE test
                   SET test.* = r_customer.*
                 WHERE id = r_customer.id
            END IF
            LET m_customer.* = r_customer.*
            LET lReturnStatus = TRUE
        CATCH
            LET lReturnStatus = FALSE
            ERROR "An error occurred while saving the data"
        END TRY
    END IF
    
    RETURN lReturnStatus

END FUNCTION #inputCustomer

PUBLIC FUNCTION loadStateCombo(pComboBox ui.ComboBox) RETURNS ()
    DISPLAY "We are running loadStateCombo"
    CALL comboboxHelper.loadStateCombo(pComboBox)
END FUNCTION #loadStateCombo
FUNCTION display_custarr()
  DEFINE cust_arr DYNAMIC ARRAY OF RECORD
     store_num     LIKE customer.store_num,
     store_name    LIKE customer.store_name,
     city          LIKE customer.city,
     state         LIKE customer.state,
     zipcode       LIKE customer.zipcode,
     contact_name  LIKE customer.contact_name,
     phone         LIKE customer.phone
    END RECORD, 
     ret_num       LIKE customer.store_num,
     ret_name      LIKE customer.store_name,
     curr_pa, idx  SMALLINT
  
  
  OPEN WINDOW wcust WITH FORM "manycust"
 
  DECLARE custlist_curs CURSOR FOR
    SELECT store_num, 
          store_name, 
          city, 
          state, 
          zipcode, 
          contact_name, 
          phone
        FROM customer
  
  LET idx = 1
  WHENEVER ERROR CONTINUE
  FOREACH custlist_curs 
       INTO cust_arr[idx].*
    LET idx = idx + 1
  END FOREACH
  WHENEVER ERROR STOP
    
   IF (idx > 1) THEN 
     CALL cust_arr.deleteElement(idx)
     LET idx = idx -1 
     DISPLAY ARRAY cust_arr TO sa_cust.* 
         ATTRIBUTES(COUNT=idx)
     LET curr_pa = arr_curr()
     LET ret_num = cust_arr[curr_pa].store_num
     LET ret_name = cust_arr[curr_pa].store_name
   ELSE
       LET ret_num = 0
       LET ret_name = "   "
   END IF   
 
  
  IF (int_flag) THEN
     LET int_flag = FALSE
     LET ret_num = 0
     LET ret_name = "   "
  END IF
 

  CLOSE WINDOW wcust
  RETURN ret_num, ret_name 

END FUNCTION -- display_cust --

