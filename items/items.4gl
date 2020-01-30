IMPORT util
IMPORT FGL comboboxHelper
IMPORT FGL DyanamicLib
IMPORT FGL UIHelper
IMPORT FGL inputFactory
IMPORT FGL input_stock
SCHEMA mydb

CONSTANT cBaseSQL = "SELECT * FROM items"
CONSTANT cOrderBy = ""
DEFINE m_items RECORD LIKE items.*
 DEFINE lCode INT 

DEFINE cust_arr DYNAMIC ARRAY OF RECORD
     order_num    LIKE items.order_num,
     stock_num   LIKE items.stock_num,
     quantity    LIKE items.quantity,
     price     LIKE items.price
     
    END RECORD


    
DEFINE idx INT 
DEFINE fetchAction RECORD
    previousRecord INTEGER,
    nextRecord INTEGER
    
END RECORD = (
    previousRecord: 0,
    nextRecord: 1
   
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
    OPEN WINDOW inputForm WITH FORM "Items_input"
    CLOSE WINDOW SCREEN
    CALL ui.Window.getCurrent().getForm().loadToolBar("RecordManager")
     CALL searchorders()
     
    CLOSE WINDOW inputForm
    
END MAIN


PRIVATE FUNCTION searchorders() 
    DEFINE lWherePart STRING
    DEFINE r_items RECORD LIKE items.*
    
   DISPLAY "Search Mode" TO formonly.mode_label
    LET lWherePart = DyanamicLib.dynamicConstruct(base.TypeInfo.create(r_items))
    IF lWherePart.getLength() > 0 THEN
        IF buildQuery(lWherePart) THEN
            CALL vieworders()    
        ELSE
            ERROR "No matches found"
        END IF
    END IF

END FUNCTION

PRIVATE FUNCTION buildQuery(pWherePart STRING) 
    DEFINE lSQLSelect STRING
    DEFINE lReturnStatus BOOLEAN = TRUE

    IF pWherePart MATCHES " WHER*" THEN
        LET lSQLSelect = cBaseSQL, pWherePart
    ELSE
        LET lSQLSelect = SFMT("%1\n WHER %2 ORDER BY %3", cBaseSQL, pWherePart, cOrderBy)
    END IF
    TRY
        DECLARE cursorderss SCROLL CURSOR FROM lSQLSelect
        OPEN cursorderss
        FETCH FIRST cursorderss INTO m_items.*
        IF SQLCA.SQLCODE > 0 THEN
            CALL closeordersCursor()
        END IF
    CATCH
        CALL closeordersCursor()
    END TRY

    RETURN lReturnStatus

END FUNCTION #buildQuery


PRIVATE FUNCTION vieworders() RETURNS ()
    LET int_flag = FALSE
     WHILE int_flag == FALSE
    
        DISPLAY m_items.* TO s_order.*
        DISPLAY "View Mode" TO formonly.mode_label
        
    MENU
            ON ACTION CANCEL
                LET int_flag = TRUE
                EXIT MENU
            
            ON ACTION PREVIOUS
                IF fetchRecord(fetchAction.previousRecord) THEN
                    EXIT MENU
                END IF
                
            ON ACTION NEXT
                IF fetchRecord(fetchAction.nextRecord) THEN
                    EXIT MENU
                END IF
            
            ON ACTION add_rec
                IF inputorders(inputMode.addMode) THEN
                    EXIT MENU
                END IF
            --ON ACTION Item
            --   CALL query_fun()
                
            ON ACTION edit_rec
                IF inputorders(inputMode.changeMode) THEN
                    EXIT MENU
                END IF
                
            --ON ACTION del_rec
            --    IF UIHelper.deleteConfirmation() THEN
            --     CALL cust_arr.deleteElement(m_orders.store_num>1)
            --        #Should delete here, but I won't
            --        IF fetchRecord(fetchAction.previousRecord) THEN
            --            EXIT MENU
            --        END IF
            --    END IF
         ON ACTION del_rec
             IF (delete_check()) THEN
             CALL delete_cust()
           END IF
           
                
        ON ACTION VIEW  
            OPEN WINDOW Display_Array WITH FORM "items"
                 CALL load_fun() RETURNING idx
                   IF idx > 1 THEN
                      CALL inpupd_fun(idx)
                   END IF
             CLOSE WINDOW Display_Array
  END MENU

    END WHILE
    LET int_flag  = FALSE

END FUNCTION #vieworders

PRIVATE FUNCTION fetchRecord(pFetchAction INTEGER) RETURNS BOOLEAN
    DEFINE lReturnStatus BOOLEAN = FALSE

    TRY
        CASE pFetchAction
           
            WHEN fetchAction.previousRecord
                FETCH PREVIOUS cursorderss INTO m_items.*
            WHEN fetchAction.nextRecord
                FETCH NEXT cursorderss INTO m_items.*

            OTHERWISE
                RETURN lReturnStatus
        END CASE
        LET lReturnStatus = (SQLCA.sqlcode == 0)
    CATCH
        LET lReturnStatus = FALSE
    END TRY
   
    RETURN lReturnStatus

END FUNCTION #fetchRecord

PRIVATE FUNCTION closeordersCursor() RETURNS ()

    TRY
        CLOSE cursorderss
    CATCH
        #Ignore errors on close cursor
    END TRY

END FUNCTION #closeordersCursor

PUBLIC FUNCTION inputorders(pMode INTEGER) 
    DEFINE lReturnStatus BOOLEAN = FALSE
    DEFINE r_items RECORD LIKE items.*
    DEFINE lStoreNum LIKE items.order_num
     --DEFINE lCode LIKE stock.fac_code

    #Do Input Statement...
    IF pMode == inputMode.changeMode THEN
        LET r_items.* = m_items.*
        DISPLAY "Change mode" TO formonly.mode_label
    ELSE
        LET r_items.order_num = 0
        DISPLAY "Add mode" TO formonly.mode_label
    END IF


    INPUT r_items.* WITHOUT DEFAULTS FROM s_order.*
      ATTRIBUTES(UNBUFFERED, ACCEPT=FALSE)
        BEFORE INPUT
            IF pMode == inputMode.changeMode THEN
                CALL DIALOG.setFieldActive("s_order.order_num", FALSE)
            END IF
        AFTER FIELD order_num
            IF pMode == inputMode.addMode AND (r_items.order_num) > 0 THEN
                SELECT order_num INTO lStoreNum
                  FROM items WHERE order_num = r_items.order_num
                IF SQLCA.SQLCODE == 0 AND LENGTH(lStoreNum) > 0 THEN
                    #Store number record already exists
                    ERROR "Store number already exists in the database"
                    NEXT FIELD order_num
                END IF
            END IF
             ON ACTION zoom1
                
            IF INFIELD(stock_num) THEN
           
                LET lCode = input_stock.stockList()
                IF LENGTH(lCode) > 0 THEN
                    LET r_items.stock_num = lCode
                END IF
            END IF
              
            
        ON ACTION save
            ACCEPT INPUT
        ON ACTION CANCEL
            LET int_flag = TRUE
            EXIT INPUT
               
    END INPUT

    IF int_flag THEN

    ELSE
        TRY
            IF pMode == inputMode.addMode THEN
                INSERT INTO items VALUES(r_items.*)
            ELSE
                UPDATE items
                   SET items.* = r_items.*
                 WHERE order_num = r_items.order_num
            END IF
            LET m_items.* = r_items.*
            LET lReturnStatus = TRUE
        CATCH
            LET lReturnStatus = FALSE
            ERROR "An error occurred while saving the data"
        END TRY
    END IF
    
    RETURN lReturnStatus

END FUNCTION #inputorders



----------------------------Delete function()--------------

FUNCTION delete_cust()
  DEFINE del_ok SMALLINT
  
    WHENEVER ERROR CONTINUE
    DELETE FROM items WHERE order_num = m_items.order_num
    WHENEVER ERROR STOP

    IF SQLCA.SQLCODE = 0 THEN
       MESSAGE "Row deleted"
       INITIALIZE m_items.* TO NULL
       DISPLAY BY NAME m_items.* 
    ELSE
      ERROR SQLERRMESSAGE
    END IF 
 
 END FUNCTION   

FUNCTION delete_check()
  DEFINE del_ok SMALLINT,
         del_count SMALLINT
  
  LET del_ok = FALSE
    
  WHENEVER ERROR CONTINUE
  SELECT COUNT(*) INTO del_count FROM items 
    WHERE items.store_num = mr_custrec.store_num
  WHENEVER ERROR STOP
  
  IF del_count > 0 THEN
    MESSAGE "Store has orders and cannot be deleted"
    LET del_ok = FALSE
  ELSE
    MENU  "Delete" ATTRIBUTE ( STYLE="dialog", COMMENT="Delete the row?" )
    COMMAND "Yes"
      LET del_ok = TRUE
      EXIT MENU
    COMMAND "No"
      MESSAGE "Delete canceled"
      EXIT MENU
    END MENU
  END IF
    
  RETURN del_ok
  
END FUNCTION

-----------------------#view function()------------------------------
FUNCTION load_fun()
  DEFINE idx     SMALLINT

  DECLARE custlist_curs CURSOR FOR
   SELECT * FROM items ORDER BY order_num
   LET idx = 1
  WHENEVER ERROR CONTINUE
  FOREACH custlist_curs INTO cust_arr[idx].*
     LET idx = idx + 1
  END FOREACH
  WHENEVER ERROR STOP
    
  IF (idx > 1) THEN 
    CALL cust_arr.deleteElement(idx)
    LET idx = idx +1 
  ELSE
    MESSAGE "No rows loaded." 
  END IF   

  RETURN idx

END FUNCTION


FUNCTION inpupd_fun(idx)
  DEFINE idx SMALLINT
         
  INPUT ARRAY cust_arr WITHOUT DEFAULTS FROM s_items.* 
  
END FUNCTION


FUNCTION stock_function()
TYPE t_stock_num LIKE stock.stock_num
DEFINE  item_index INT 
 DEFINE lCode INT 
DEFINE arr1 DYNAMIC ARRAY OF RECORD
     stock_num    LIKE stock.stock_num,
     fac_code   LIKE stock.fac_code
       END RECORD
 
        DECLARE t_cur SCROLL CURSOR FOR
          SELECT * FROM stock
                 
          OPEN t_cur 
          --CALL  arr1.clear()
          LET item_index = 1
      FOREACH t_cur INTO arr1[item_index].*
        LET item_index = item_index + 1
      END FOREACH
       IF arr1.getLength() > 0 THEN
        DISPLAY ARRAY arr1 TO src.*
            ON ACTION CANCEL
                LET int_flag = TRUE
                EXIT DISPLAY
            AFTER DISPLAY
                LET item_index = arr_curr()
                LET lCode = arr1[item_index].stock_num
        END DISPLAY
    ELSE
        CALL UIHelper.informationDialog("No Records", "No factory records in the database")
    END IF
        
        DISPLAY ARRAY arr1 TO src.*

  END FUNCTION  

 