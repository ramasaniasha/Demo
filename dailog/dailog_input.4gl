IMPORT util
IMPORT FGL DynamicUI
IMPORT FGL UIHelper
SCHEMA mydb
CONSTANT cBaseSQL = "SELECT * FROM customer"

TYPE t_customer RECORD LIKE customer.*
DEFINE customerList DYNAMIC ARRAY OF t_customer
MAIN
    DATABASE mydb
    CALL customerSearchList()
   -- CALL dynamicCustomerSearchList()
    
END MAIN

PRIVATE FUNCTION customerSearchList() RETURNS ()
    DEFINE lSQLWhere STRING

    OPEN WINDOW customerSearchWindow WITH FORM "dailog"
     ATTRIBUTES(TEXT="Customer Search List")
    CLOSE WINDOW SCREEN
    
    LET int_flag = FALSE
    WHILE int_flag == FALSE

        DIALOG ATTRIBUTES(UNBUFFERED)
        
            CONSTRUCT lSQLWhere ON customer.* FROM s_customer.*
                ON ACTION do_search
                    #Need to force the focus to the display array
                    CALL DIALOG.nextField("d_customer.t_store_name")
                ON ACTION CLEAR
                CLEAR customer.*
                               
                 
            END CONSTRUCT

           DISPLAY ARRAY customerList TO d_customer.*
                
                BEFORE DISPLAY
                    DISPLAY SFMT("%1\n WHERE %2", cBaseSQL, lSQLWhere) TO formonly.sql_query
                    CALL buildCustomerList(lSQLWhere)
                ON ACTION dc_action
                              
                    DISPLAY "Double-Click!"
                      
                ON ACTION testing
                    DISPLAY "Testing"
            END DISPLAY

            BEFORE DIALOG
                CALL DIALOG.getForm().setFieldHidden("d_customer.t_contact_name", 1)

            ON ACTION CANCEL
                LET int_flag = TRUE
                EXIT DIALOG

            AFTER DIALOG
                CONTINUE DIALOG

        END DIALOG
        
        IF int_flag == FALSE THEN
            DISPLAY SFMT("%1\n WHERE %2", cBaseSQL, lSQLWhere) TO formonly.sql_query
        END IF

    END WHILE
    
    CLOSE WINDOW customerSearchWindow

END FUNCTION

PRIVATE FUNCTION dynamicCustomerSearchList() RETURNS ()
    DEFINE r_customer t_customer
    DEFINE lRecordNode om.DomNode
    DEFINE callback DynamicUI.t_searchListCallback

    LET lRecordNode = base.TypeInfo.create(r_customer)
    LET callback.getDialogTriggers = FUNCTION getDialogTriggers
    LET callback.handleDialogEvents = FUNCTION handleDisplayArrayEvents
    LET callback.executeSearch = FUNCTION executeSearch
    LET callback.enableMultiSelect = FUNCTION enableMultiSelect

    OPEN WINDOW customerSearchWindow WITH FORM "dailog"
     ATTRIBUTES(TEXT="Customer Search List")
    CLOSE WINDOW SCREEN

    CALL DynamicUI.dynamicSearchList(lRecordNode, "d_customer", "t_", callback.*)

    CLOSE WINDOW customerSearchWindow
   
END FUNCTION

PUBLIC FUNCTION getDialogTriggers() RETURNS DYNAMIC ARRAY OF DynamicUI.t_dialogEvent
    DEFINE lTriggerList DYNAMIC ARRAY OF t_dialogEvent
    DEFINE lIndex INTEGER = 0

     LET lIndex = lIndex + 1
    LET lTriggerList[lIndex].dialog_type = DynamicUI.dialogTypes.displayArrayType
    LET lTriggerList[lIndex].dialog_event = "ON ACTION clear"


    LET lIndex = lIndex + 1
    LET lTriggerList[lIndex].dialog_type = DynamicUI.dialogTypes.displayArrayType
    LET lTriggerList[lIndex].dialog_event = "ON ACTION dc_action"

    LET lIndex = lIndex + 1
    LET lTriggerList[lIndex].dialog_type = DynamicUI.dialogTypes.displayArrayType
    LET lTriggerList[lIndex].dialog_event = "ON ACTION testing"

    LET lIndex = lIndex + 1
    LET lTriggerList[lIndex].dialog_type = DynamicUI.dialogTypes.displayArrayType
    LET lTriggerList[lIndex].dialog_event = "ON ACTION refresh"
    
    LET lIndex = lIndex + 1
    LET lTriggerList[lIndex].dialog_type = DynamicUI.dialogTypes.displayArrayType
    LET lTriggerList[lIndex].dialog_event = "ON DELETE"

    LET lIndex = lIndex + 1
    LET lTriggerList[lIndex].dialog_type = DynamicUI.dialogTypes.displayArrayType
    LET lTriggerList[lIndex].dialog_event = "ON DELETE"

    RETURN lTriggerList
    
END FUNCTION #getDisplayArrayTriggers

PUBLIC FUNCTION handleDisplayArrayEvents(pDialog ui.Dialog, pEvent STRING, pCurrentRow INTEGER) RETURNS BOOLEAN

    CASE pEvent
        --WHEN "ON ACTION clear"
        --       LET int_flag = TRUE CLEAR customer.*
        WHEN "ON ACTION d_customer.d_customer.dc_action"
            CALL UIHelper.informationDialog("Double-Click", util.JSONObject.fromFGL(customerList[pCurrentRow]).toString())
        WHEN "ON ACTION d_customer.testing"
            CALL UIHelper.informationDialog("Testing", "Testing action triggered")
        WHEN "ON DELETE d_customer"
            LET int_flag = NOT UIHelper.deleteConfirmation()
        WHEN "AFTER DISPLAY d_customer"
            --CALL UIHelper.informationDialog("Accept/OK", util.JSONObject.fromFGL(customerList[pCurrentRow]).toString())
        OTHERWISE
            DISPLAY SFMT("Unhandled Event: %1", pEvent)
    END CASE
    RETURN TRUE
        
END FUNCTION #handleDisplayArrayEvents

PUBLIC FUNCTION executeSearch(pWherePart STRING) RETURNS util.JSONArray

    CALL buildCustomerList(pWherePart)
    DISPLAY SFMT("%1\n%2", cBaseSQL, pWherePart) TO formonly.sql_query
    RETURN util.JSONArray.fromFGL(customerList)

END FUNCTION

PUBLIC FUNCTION enableMultiSelect() RETURNS BOOLEAN
    RETURN TRUE
END FUNCTION

PRIVATE FUNCTION buildCustomerList(pWherePart STRING) RETURNS ()
    DEFINE lSQLSelect STRING
    DEFINE idx INTEGER
    DEFINE lCustomer t_customer

    CALL customerList.clear()
    IF pWherePart MATCHES " WHERE*" THEN
        LET lSQLSelect = cBaseSQL, pWherePart
    ELSE
        LET lSQLSelect = SFMT("%1\n WHERE %2", cBaseSQL, pWherePart)
    END IF
    DECLARE cursCustomers CURSOR FROM lSQLSelect
    FOREACH cursCustomers INTO lCustomer.*
        LET idx = idx + 1
        LET customerList[idx].* = lCustomer.*
    END FOREACH

END FUNCTION

PUBLIC FUNCTION loadStateCombo(pCombo ui.ComboBox) RETURNS ()
    DEFINE lCode LIKE state.state_code
    DEFINE lName LIKE state.state_name

    --Populate the state combobox
    DECLARE cursStates CURSOR FROM "SELECT state_code, state_name FROM state ORDER BY state_name"
    FOREACH cursStates INTO lCode, lName
        CALL pCombo.addItem(lCode, lName)
    END FOREACH

END FUNCTION