IMPORT util
IMPORT FGL DynamicUI
IMPORT FGL UIHelper
SCHEMA mydb

CONSTANT cBaseSQL = "SELECT * FROM orders"
CONSTANT cOrderBy = "order_num"
DEFINE m_orders RECORD LIKE orders.*
DEFINE order_num  LIKE orders.order_num,
       order_date LIKE orders.order_date 

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

    --WHENEVER ANY ERROR CALL errorHandler
    --CALL STARTLOG("customerInput.log")
    DATABASE mydb

    OPEN WINDOW inputForm WITH FORM "dailog_ex1"
    CLOSE WINDOW SCREEN
    CALL ui.Window.getCurrent().getForm().loadToolBar("RecordManager")

    WHILE searchCustomer()
        DISPLAY "Looping back to search"
    END WHILE
    
    CLOSE WINDOW inputForm
    
END MAIN

PRIVATE FUNCTION searchCustomer() RETURNS BOOLEAN
    DEFINE lWherePart STRING
    DEFINE r_orders RECORD LIKE orders.*
    DEFINE lReturnStatus BOOLEAN = FALSE
    
    DISPLAY "Search Mode" TO formonly.mode_label
    LET lWherePart = DynamicUI.dynamicConstruct(base.TypeInfo.create(r_orders))
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
        FETCH FIRST cursCustomers INTO m_orders.*
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
                FETCH FIRST cursCustomers INTO m_orders.*
            WHEN fetchAction.previousRecord
                FETCH PREVIOUS cursCustomers INTO m_orders.*
            WHEN fetchAction.nextRecord
                FETCH NEXT cursCustomers INTO m_orders.*
            WHEN fetchAction.lastRecord
                FETCH LAST cursCustomers INTO m_orders.*
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

        DISPLAY m_orders.* TO scr.*
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
                IF fetchRecord(fetchAction.previousRecord) THEN
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
                ON ACTION VIEW
                 CALL display_custarr()
                 RETURNING order_num, order_date
                 DISPLAY order_num, order_date
       
            ON ACTION del_rec
            IF delete_order() THEN 
            
            END IF
            ON ACTION cancle 
            EXIT MENU 
        END MENU

    END WHILE
    LET int_flag  = FALSE

END FUNCTION #viewCustomer

PUBLIC FUNCTION inputCustomer(pMode INTEGER) RETURNS BOOLEAN
    DEFINE lReturnStatus BOOLEAN = FALSE
    DEFINE r_orders RECORD LIKE orders.*
    DEFINE lStoreNum LIKE orders.order_num
    #Do Input Statement...
    IF pMode == inputMode.changeMode THEN
        LET r_orders.* = m_orders.*
        DISPLAY "Change mode" TO formonly.mode_label
    ELSE
        LET r_orders.order_num = 0
        DISPLAY "Add mode" TO formonly.mode_label
    END IF


    INPUT r_orders.* WITHOUT DEFAULTS FROM scr.*
     ATTRIBUTES(UNBUFFERED, ACCEPT=FALSE)
        BEFORE INPUT
            IF pMode == inputMode.changeMode THEN
                CALL DIALOG.setFieldActive("scr.order_num", FALSE)
            END IF
        AFTER FIELD store_num
            IF pMode == inputMode.addMode AND (r_orders.store_num) > 0 THEN
                SELECT store_num INTO lStoreNum
                  FROM orders WHERE order_num = r_orders.order_num
                IF SQLCA.SQLCODE == 0 AND LENGTH(lStoreNum) > 0 THEN
                    #Store number record already exists
                    ERROR "Order number already exists in the database"
                    NEXT FIELD store_num
                END IF
            END IF
       
        ON ACTION save
            ACCEPT INPUT
        ON ACTION CANCEL
            LET int_flag = TRUE
            EXIT INPUT
        END INPUT

    IF int_flag THEN
        LET lReturnStatus = FALSE
    ELSE
        TRY
            IF pMode == inputMode.addMode THEN
                INSERT INTO orders VALUES(r_orders.*)
            ELSE
                UPDATE orders
                   SET orders.* = r_orders.*
                 WHERE order_num = r_orders.order_num
            END IF
            LET m_orders.* = r_orders.*
            LET lReturnStatus = TRUE
        CATCH
            LET lReturnStatus = FALSE
            ERROR "An error occurred while saving the data"
        END TRY
    END IF
    
    RETURN lReturnStatus

END FUNCTION #inputCustomer
FUNCTION display_custarr()
  DEFINE cust_arr DYNAMIC ARRAY OF RECORD
     order_num     LIKE orders.order_num,
     order_date    LIKE orders.order_date,
     store_num     LIKE orders.store_num,
     fac_code      LIKE orders.fac_code,
     ship_instr    LIKE orders.ship_instr,
     promo  LIKE orders.promo    
    END RECORD, 
     ret_num       LIKE orders.order_num,
     ret_date     LIKE orders.order_date,
     curr_pa, idx  SMALLINT
   
  OPEN WINDOW wcust WITH FORM "manycust"
 
  DECLARE custlist_curs CURSOR FOR
    SELECT order_num, 
          order_date, 
          store_num, 
          fac_code, 
          ship_instr 
         FROM orders
  
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
     LET ret_num = cust_arr[curr_pa].order_num
     LET ret_date = cust_arr[curr_pa].order_date
   ELSE
       LET ret_num = 0
       LET ret_date = "   "
   END IF   
 
  
  IF (int_flag) THEN
     LET int_flag = FALSE
     LET ret_num = 0
     LET ret_date = "   "
  END IF
 

  CLOSE WINDOW wcust
  RETURN ret_num, ret_date 

END FUNCTION -- display_cust --
FUNCTION delete_order()
  DEFINE idx     SMALLINT,
         del_ok  SMALLINT
       
      IF del_ok = TRUE THEN 
        WHENEVER ERROR CONTINUE
        DELETE  FROM orders 
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

