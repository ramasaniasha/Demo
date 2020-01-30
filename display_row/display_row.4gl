SCHEMA mydb 
MAIN 
DEFINE rec RECORD 
      --store_num LIKE customer.store_num,
      store_name LIKE customer.store_name,
      phone LIKE customer.phone 
      END RECORD 
      CONNECT TO "mydb"
      OPEN FORM f1 FROM "display"
      DISPLAY FORM f1 
      MENU 
      ON ACTION get 
      SELECT store_name,phone INTO rec.* FROM customer
      WHERE store_num = 101
      DISPLAY BY NAME rec.*
      ON ACTION CLOSE
      EXIT MENU 
      END MENU 
      DISCONNECT CURRENT 

END MAIN 