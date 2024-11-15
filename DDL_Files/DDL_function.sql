-- Function Definitions
CREATE OR REPLACE FUNCTION store_demo_db.store_demo_sch.ADD_CUSTOMER(
  p_FIRST_NAME VARCHAR2,
  p_LAST_NAME VARCHAR2,
  p_EMAIL VARCHAR2,
  p_ADDRESS VARCHAR2,
  p_PHONE NUMBER
) RETURN NUMBER AS
  NEW_CUSTOMER_ID NUMBER;
BEGIN
  INSERT INTO store_demo_db.store_demo_sch.CUSTOMERS (FIRST_NAME, LAST_NAME, EMAIL, ADDRESS, PHONE)
  VALUES (p_FIRST_NAME, p_LAST_NAME, p_EMAIL, p_ADDRESS, p_PHONE)
  RETURNING CUSTOMER_ID INTO NEW_CUSTOMER_ID;
  RETURN NEW_CUSTOMER_ID;
END ADD_CUSTOMER;

CREATE OR REPLACE FUNCTION store_demo_db.store_demo_sch.ADD_PRODUCT(
  p_PRODUCT_NAME VARCHAR2,
  p_DESCRIPTION VARCHAR2,
  p_PRICE NUMBER,
  p_STOCK_QUANTITY NUMBER
) RETURN NUMBER AS
  NEW_PRODUCT_ID NUMBER;
BEGIN
  INSERT INTO store_demo_db.store_demo_sch.PRODUCTS (PRODUCT_NAME, DESCRIPTION, PRICE, STOCK_QUANTITY)
  VALUES (p_PRODUCT_NAME, p_DESCRIPTION, p_PRICE, p_STOCK_QUANTITY)
  RETURNING PRODUCT_ID INTO NEW_PRODUCT_ID;
  RETURN NEW_PRODUCT_ID;
END ADD_PRODUCT;

CREATE OR REPLACE FUNCTION store_demo_db.store_demo_sch.PLACE_ORDER(
  p_CUSTOMER_ID NUMBER,
  p_ORDER_DATE DATE,
  p_TOTAL NUMBER
) RETURN NUMBER AS
  NEW_ORDER_ID NUMBER;
BEGIN
  INSERT INTO store_demo_db.store_demo_sch.ORDERS (CUSTOMER_ID, ORDER_DATE, TOTAL)
  VALUES (p_CUSTOMER_ID, p_ORDER_DATE, p_TOTAL)
  RETURNING ORDER_ID INTO NEW_ORDER_ID;
  RETURN NEW_ORDER_ID;
END PLACE_ORDER;

CREATE OR REPLACE FUNCTION store_demo_db.store_demo_sch.ADD_ORDER_ITEM(
  p_ORDER_ID NUMBER,
  p_PRODUCT_ID NUMBER,
  p_QUANTITY NUMBER,
  p_UNIT_PRICE NUMBER
) RETURN NUMBER AS
  NEW_ORDER_ITEM_ID NUMBER;
BEGIN
  INSERT INTO store_demo_db.store_demo_sch.ORDER_ITEMS (ORDER_ID, PRODUCT_ID, QUANTITY, UNIT_PRICE)
  VALUES (p_ORDER_ID, p_PRODUCT_ID, p_QUANTITY, p_UNIT_PRICE)
  RETURNING ORDER_ITEM_ID INTO NEW_ORDER_ITEM_ID;
  RETURN NEW_ORDER_ITEM_ID;
END ADD_ORDER_ITEM;

CREATE OR REPLACE FUNCTION store_demo_db.store_demo_sch.MAKE_PAYMENT(
  p_ORDER_ID NUMBER,
  p_PAYMENT_DATE DATE,
  p_PAYMENT_METHOD VARCHAR2,
  p_AMOUNT NUMBER
) RETURN NUMBER AS
  NEW_PAYMENT_ID NUMBER;
BEGIN
  INSERT INTO store_demo_db.store_demo_sch.PAYMENTS (ORDER_ID, PAYMENT_DATE, PAYMENT_METHOD, AMOUNT)
  VALUES (p_ORDER_ID, p_PAYMENT_DATE, p_PAYMENT_METHOD, p_AMOUNT)
  RETURNING PAYMENT_ID INTO NEW_PAYMENT_ID;
  RETURN NEW_PAYMENT_ID;
END MAKE_PAYMENT;