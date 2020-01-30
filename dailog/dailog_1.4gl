IMPORT util
IMPORT FGL DynamicUI
IMPORT FGL UIHelper
SCHEMA mydb
TYPE rec RECORD LIKE customer.*
DEFINE cust_list DYNAMIC ARRAY OF rec

MAIN 
OPEN WINDOW w1 WITH FORM "dailog_1"
 CALL customer_list()
CLOSE WINDOW SCREEN 
END MAIN 
PRIVATE FUNCTION customer_list() RETURNS ()
DEFINE lsql STRING 
LET int_flag = FALSE 
WHILE int_flag == FALSE 
LET lsql = "select * from customer"

 CONSTRUCT lsql ON customer.* FROM s_customer.*
                ON ACTION do_search
                    #Need to force the focus to the display array
                    CALL DIALOG.nextField("d_customer.rec")
                ON ACTION CLEAR
                CLEAR customer.*
                                    
            END CONSTRUCT
END WHILE 
END FUNCTION  


