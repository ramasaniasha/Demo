IMPORT util
IMPORT FGL DynamicUI
IMPORT FGL UIHelper
IMPORT FGL enums
SCHEMA mydb

CONSTANT cBaseSQL = "SELECT stock_num FROM stock"
CONSTANT cOrderBy = "stock_num"

TYPE t_stock RECORD 
          stock_num LIKE stock.stock_num,
          fac_code LIKE stock.fac_code
          END RECORD 
TYPE t_stock_num LIKE stock.stock_num

DEFINE m_stock t_stock
DEFINE m_codeList DYNAMIC ARRAY OF t_stock_num
DEFINE mCurrentRow INTEGER = 0
DEFINE mCursorDefined BOOLEAN = FALSE

MAIN

    --WHENEVER ANY ERROR CALL errorHandler
    --CALL STARTLOG("stockInput.log")
    DATABASE mydb

    OPEN WINDOW inputForm WITH FORM "stock"
    CLOSE WINDOW SCREEN
    CALL ui.Window.getCurrent().getForm().loadToolBar("RecordManager")

    WHILE searchstock()
        DISPLAY "Looping back to search"
    END WHILE
    
    CLOSE WINDOW inputForm
    
END MAIN

PRIVATE FUNCTION searchstock() RETURNS BOOLEAN
    DEFINE lWherePart STRING
    DEFINE lReturnStatus BOOLEAN = FALSE
    
    DISPLAY "Search Mode" TO formonly.mode_label
    LET lWherePart = DynamicUI.dynamicConstruct(base.TypeInfo.create(m_stock))
    IF lWherePart.getLength() > 0 THEN
        IF buildQuery(lWherePart) THEN
            CALL viewstock()
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
    DEFINE lCode t_stock_num

    IF pWherePart MATCHES " WHERE*" THEN
        LET lSQLSelect = cBaseSQL, pWherePart
    ELSE
        LET lSQLSelect = SFMT("%1\n WHERE %2 ORDER BY %3", cBaseSQL, pWherePart, cOrderBy)
    END IF
    TRY
        LET mCurrentRow = 0
        CALL m_codeList.clear()
        DECLARE cursstockCode CURSOR FROM lSQLSelect
        FOREACH cursstockCode INTO lCode
            LET mCurrentRow = mCurrentRow + 1
            LET m_codeList[mCurrentRow] = lCode
        END FOREACH
        IF m_codeList.getLength() > 0 THEN
            LET lReturnStatus = fetchRecord(fetchAction.firstRecord)
        ELSE
            LET lReturnStatus = FALSE
        END IF
    CATCH
        LET lReturnStatus = FALSE
    END TRY

    RETURN lReturnStatus

END FUNCTION #buildQuery

PRIVATE FUNCTION fetchRecord(pFetchAction INTEGER) RETURNS BOOLEAN
    DEFINE lReturnStatus BOOLEAN = FALSE

    IF mCursorDefined == FALSE THEN
        DECLARE cursstockByCode CURSOR FROM "SELECT * FROM stock WHERE stock_num = ?"
        LET mCursorDefined = TRUE
    END IF

    CASE pFetchAction
        WHEN fetchAction.firstRecord
            LET mCurrentRow = 1
        WHEN fetchAction.previousRecord
            LET mCurrentRow = IIF(mCurrentRow > 1, mCurrentRow - 1, 1)
        WHEN fetchAction.nextRecord
            LET mCurrentRow = IIF(mCurrentRow < m_codeList.getLength(), mCurrentRow + 1, m_codeList.getLength())
        WHEN fetchAction.lastRecord
            LET mCurrentRow = m_codeList.getLength()
        OTHERWISE
            IF mCurrentRow < 1 THEN
                LET mCurrentRow = 1
            ELSE
                IF mCurrentRow > m_codeList.getLength() THEN
                    LET mCurrentRow = m_codeList.getLength()
                END IF
            END IF
    END CASE

    TRY
        OPEN cursstockByCode USING m_codeList[mCurrentRow]
        FETCH cursstockByCode INTO m_stock.*
        LET lReturnStatus = (SQLCA.sqlcode == 0)
        CLOSE cursstockByCode
    CATCH
        LET lReturnStatus = FALSE
    END TRY

    RETURN lReturnStatus

END FUNCTION #fetchRecord

PRIVATE FUNCTION viewstock() RETURNS ()

    LET int_flag = FALSE
    WHILE int_flag == FALSE

        DISPLAY m_stock.* TO s_stock.*
        DISPLAY SFMT("View Mode %1 of %2", mCurrentRow, m_codeList.getLength()) TO formonly.mode_label
        
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
                IF inputstock(inputMode.addMode) THEN
                    IF fetchRecord(fetchAction.refreshRecord) THEN
                        EXIT MENU
                    END IF
                END IF
            ON ACTION edit_rec
                IF inputstock(inputMode.changeMode) THEN
                    IF fetchRecord(fetchAction.refreshRecord) THEN
                        EXIT MENU
                    END IF
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

END FUNCTION #viewstock

PUBLIC FUNCTION inputstock(pMode INTEGER) RETURNS BOOLEAN
    DEFINE lReturnStatus BOOLEAN = FALSE
    DEFINE r_stock t_stock
    DEFINE lCode t_stock_num

    #Do Input Statement...
    IF pMode == inputMode.changeMode THEN
        LET r_stock.* = m_stock.*
        DISPLAY "Change mode" TO formonly.mode_label
    ELSE
        DISPLAY "Add mode" TO formonly.mode_label
    END IF

    INPUT r_stock.* WITHOUT DEFAULTS FROM s_stock.*
     ATTRIBUTES(UNBUFFERED, ACCEPT=FALSE)
        BEFORE INPUT
            IF pMode == inputMode.changeMode THEN
                CALL DIALOG.setFieldActive("s_stock.stock_num", FALSE)
            END IF
        AFTER FIELD stock_num
            IF pMode == inputMode.addMode AND (r_stock.stock_num) > 0 THEN
                SELECT stock_num INTO lCode
                  FROM stock WHERE stock_num = r_stock.stock_num
                IF SQLCA.SQLCODE == 0 AND LENGTH(lCode) > 0 THEN
                    #stock record already exists for key
                    ERROR "stock code already exists in the database"
                    NEXT FIELD stock_num
                END IF
            END IF
        ON ACTION save
            ACCEPT INPUT
        ON ACTION CANCEL
            LET int_flag = TRUE
            EXIT INPUT
        AFTER INPUT
            IF LENGTH(r_stock.fac_code) == 0 THEN
                ERROR "stock name is missing"
                NEXT FIELD fac_code
            END IF
    END INPUT

    --IF int_flag THEN
    --    LET lReturnStatus = FALSE
    --ELSE
    --    TRY
    --        IF pMode == inputMode.addMode THEN
    --            INSERT INTO stock VALUES(r_stock.*)
    --            CALL m_codeList.appendElement()
    --            LET mCurrentRow = m_codeList.getLength()
    --            LET m_codeList[mCurrentRow] = r_stock.stock_num
    --        ELSE
    --            UPDATE stock
    --               SET stock.* = r_stock.*
    --             WHERE stock_num = r_stock.stock_num
    --        END IF
    --        LET m_stock.* = r_stock.*
    --        LET lReturnStatus = TRUE
    --    CATCH
    --        LET lReturnStatus = FALSE
    --        ERROR "An error occurred while saving the data"
    --    END TRY
    --END IF
    LET int_flag = FALSE
    RETURN lReturnStatus

END FUNCTION #inputstock

PUBLIC FUNCTION stockList() RETURNS t_stock_num
    DEFINE r_stock t_stock
    DEFINE l_stockList DYNAMIC ARRAY OF t_stock
    DEFINE lIndex INTEGER
    DEFINE lCode INT 
    
    OPEN WINDOW selectForm WITH FORM "stoke_list"
     ATTRIBUTES(TEXT="stock List", STYLE="dialog")

    LET lIndex = 0
    --CALL l_stockList.clear()
    DECLARE cursFactories CURSOR FROM "SELECT stock_num,fac_code FROM stock ORDER BY stock_num"
    FOREACH cursFactories INTO r_stock.*
        LET lIndex = lIndex + 1
        LET l_stockList[lIndex].* = r_stock.*
    END FOREACH

    IF l_stockList.getLength() > 0 THEN
       DISPLAY ARRAY l_stockList TO scr.*
            ON ACTION CANCEL
                LET int_flag = TRUE
                EXIT DISPLAY
            AFTER DISPLAY
                LET lIndex = arr_curr()
                LET lCode = l_stockList[lIndex].stock_num
        END DISPLAY
    ELSE
        CALL UIHelper.informationDialog("No Records", "No stock records in the database")
    END IF
    
    CLOSE WINDOW selectForm
    LET int_flag = FALSE
    RETURN lCode
    
END FUNCTION #stockList

--PRIVATE FUNCTION errorHandler() RETURNS ()
--    DEFINE lMessage STRING
--    LET lMessage = SFMT("Error Code: %1\n%2", STATUS, err_get(STATUS))
--    CALL ERRORLOG(base.Application.getStackTrace())
--    CALL UIHelper.informationDialog("Error Occurred", lMessage)
--END FUNCTION