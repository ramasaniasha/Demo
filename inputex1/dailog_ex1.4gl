IMPORT util
IMPORT FGL comboboxHelper
IMPORT FGL DynamicUI
IMPORT FGL UIHelper
IMPORT FGL inputFactory
SCHEMA mydb

CONSTANT cBaseSQL = "SELECT * FROM orders"
CONSTANT cOrderBy = "store_num"
DEFINE m_orders RECORD LIKE orders.*
DEFINE cust_arr DYNAMIC ARRAY OF RECORD
order_num LIKE orders.order_num,
order_date LIKE orders.order_date,
store_num LIKE orders.store_num,
fac_code LIKE orders.fac_code,
ship_instr LIKE orders.ship_instr,
promo LIKE orders.promo
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

DATABASE db
OPEN WINDOW inputForm WITH FORM "ex01_input"
CLOSE WINDOW SCREEN
CALL ui.Window.getCurrent().getForm().loadToolBar("RecordManager")

CALL searchorders()
CLOSE WINDOW inputForm

END MAIN

PRIVATE FUNCTION searchorders()
DEFINE lWherePart STRING
DEFINE r_orders RECORD LIKE orders.*

DISPLAY "Search Mode" TO formonly.mode_label
LET lWherePart = DyanamicLib.dynamicConstruct(base.TypeInfo.create(r_orders))
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
FETCH FIRST cursorderss INTO m_orders.*
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

DISPLAY m_orders. TO s_order.
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
#Need to re-create cursor???
EXIT MENU
END IF
ON ACTION edit_rec
IF inputorders(inputMode.changeMode) THEN
#Need to re-create cursor???
EXIT MENU
END IF
--ON ACTION del_rec
-- IF UIHelper.deleteConfirmation() THEN
-- CALL cust_arr.deleteElement(m_orders.store_num>1)
-- #Should delete here, but I won't
-- IF fetchRecord(fetchAction.previousRecord) THEN
-- EXIT MENU
-- END IF
-- END IF
ON ACTION del_rec
IF (delete_check()) THEN
CALL delete_cust()
END IF

ON ACTION VIEW
OPEN WINDOW Display_Array WITH FORM "show"
CALL load_fun() RETURNING idx
IF idx > 1 THEN
CALL inpupd_fun(idx)
END IF
CLOSE WINDOW Display_Array
END MENU

END WHILE
LET int_flag = FALSE

END FUNCTION #vieworders

PRIVATE FUNCTION fetchRecord(pFetchAction INTEGER) RETURNS BOOLEAN
DEFINE lReturnStatus BOOLEAN = FALSE

TRY
CASE pFetchAction

WHEN fetchAction.previousRecord
FETCH PREVIOUS cursorderss INTO m_orders.*
WHEN fetchAction.nextRecord
FETCH NEXT cursorderss INTO m_orders.*

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

PUBLIC FUNCTION inputorders(pMode INTEGER) RETURNS BOOLEAN
DEFINE lReturnStatus BOOLEAN = FALSE
DEFINE r_orders RECORD LIKE orders.*
DEFINE lStoreNum LIKE orders.store_num
DEFINE lCode LIKE stock.fac_code

#Do Input Statement...
IF pMode == inputMode.changeMode THEN
LET r_orders. = m_orders.
DISPLAY "Change mode" TO formonly.mode_label
ELSE
LET r_orders.store_num = 0
DISPLAY "Add mode" TO formonly.mode_label
END IF


INPUT r_orders. WITHOUT DEFAULTS FROM s_order.
ATTRIBUTES(UNBUFFERED, ACCEPT=FALSE)
BEFORE INPUT
IF pMode == inputMode.changeMode THEN
CALL DIALOG.setFieldActive("s_order.store_num", FALSE)
END IF
AFTER FIELD store_num
IF pMode == inputMode.addMode AND (r_orders.store_num) > 0 THEN
SELECT store_num INTO lStoreNum
FROM orders WHERE store_num = r_orders.store_num
IF SQLCA.SQLCODE == 0 AND LENGTH(lStoreNum) > 0 THEN
#Store number record already exists
ERROR "Store number already exists in the database"
NEXT FIELD store_num
END IF
END IF
ON ACTION zoom
IF INFIELD(fac_code) THEN
LET lCode = inputFactory.factoryList()
IF LENGTH(lCode) > 0 THEN
LET r_orders.fac_code = lCode
END IF
END IF
ON ACTION save
ACCEPT INPUT
ON ACTION CANCEL
LET int_flag = TRUE
EXIT INPUT
AFTER INPUT
IF LENGTH(r_orders.store_num) == 0 THEN
ERROR "Store name is missing"
NEXT FIELD store_name
END IF
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
WHERE store_num = r_orders.store_num
END IF
LET m_orders.* = r_orders.*
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
DELETE FROM orders WHERE store_num = m_orders.store_num
WHENEVER ERROR STOP
IF SQLCA.SQLCODE = 0 THEN
MESSAGE "Row deleted"
INITIALIZE m_orders.* TO NULL
DISPLAY BY NAME m_orders.*
ELSE
ERROR SQLERRMESSAGE
END IF

END FUNCTION

FUNCTION delete_check()
DEFINE del_ok SMALLINT,
del_count SMALLINT

LET del_ok = FALSE

WHENEVER ERROR CONTINUE
SELECT COUNT(*) INTO del_count FROM ORDERS
WHERE orders.store_num = mr_custrec.store_num
WHENEVER ERROR STOP

IF del_count > 0 THEN
MESSAGE "Store has orders and cannot be deleted"
LET del_ok = FALSE
ELSE
MENU "Delete" ATTRIBUTE ( STYLE="dialog", COMMENT="Delete the row?" )
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
DEFINE idx SMALLINT

DECLARE custlist_curs CURSOR FOR
SELECT *
FROM orders
ORDER BY store_num

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

INPUT ARRAY cust_arr WITHOUT DEFAULTS
FROM sr.*
ATTRIBUTE (UNBUFFERED, COUNT=idx)



IF (int_flag) THEN
LET int_flag = FALSE
END IF

END FUNCTION


----------------------------------------