-- View Definitions
CREATE VIEW store_demo_db.store_demo_sch.V_CUSTOMER_ORDERS AS
SELECT 
  c.CUSTOMER_ID, 
  c.FIRST_NAME, 
  c.LAST_NAME, 
  o.ORDER_ID, 
  o.ORDER_DATE, 
  o.TOTAL
FROM store_demo_db.store_demo_sch.CUSTOMERS c
JOIN ORDERS o ON c.CUSTOMER_ID = o.CUSTOMER_ID;

CREATE VIEW store_demo_db.store_demo_sch.V_ORDER_ITEMS_DETAILS AS
SELECT 
  oi.ORDER_ITEM_ID, 
  oi.ORDER_ID, 
  oi.PRODUCT_ID, 
  p.PRODUCT_NAME, 
  oi.QUANTITY, 
  oi.UNIT_PRICE
FROM store_demo_db.store_demo_sch.ORDER_ITEMS oi
JOIN store_demo_db.store_demo_sch.PRODUCTS p ON oi.PRODUCT_ID = p.PRODUCT_ID;

CREATE VIEW store_demo_db.store_demo_sch.V_CUSTOMER_ORDER_HISTORY AS
SELECT 
  c.CUSTOMER_ID, 
  c.FIRST_NAME, 
  c.LAST_NAME, 
  o.ORDER_ID, 
  o.ORDER_DATE, 
  o.TOTAL
FROM store_demo_db.store_demo_sch.CUSTOMERS c
JOIN store_demo_db.store_demo_sch.ORDERS o ON c.CUSTOMER_ID = o.CUSTOMER_ID
ORDER BY o.ORDER_DATE DESC;

CREATE VIEW store_demo_db.store_demo_sch.V_TOP_SELLING_PRODUCTS AS
SELECT 
  p.PRODUCT_ID, 
  p.PRODUCT_NAME, 
  SUM(oi.QUANTITY) AS TOTAL_QUANTITY
FROM store_demo_db.store_demo_sch.ORDER_ITEMS oi
JOIN store_demo_db.store_demo_sch.PRODUCTS p ON oi.PRODUCT_ID = p.PRODUCT_ID
GROUP BY p.PRODUCT_ID, p.PRODUCT_NAME
ORDER BY TOTAL_QUANTITY DESC;

CREATE VIEW store_demo_db.store_demo_sch.V_CUSTOMER_PURCHASES AS
SELECT 
  c.CUSTOMER_ID, 
  c.FIRST_NAME, 
  c.LAST_NAME, 
  SUM(o.TOTAL) AS TOTAL_PURCHASES
FROM store_demo_db.store_demo_sch.CUSTOMERS c
JOIN store_demo_db.store_demo_sch.ORDERS o ON c.CUSTOMER_ID = o.CUSTOMER_ID
GROUP BY c.CUSTOMER_ID, c.FIRST_NAME, c.LAST_NAME;

CREATE VIEW store_demo_db.store_demo_sch.V_ORDER_TOTALS AS
SELECT 
  o.ORDER_ID, 
  SUM(oi.UNIT_PRICE * oi.QUANTITY) AS ORDER_TOTAL
FROM store_demo_db.store_demo_sch.ORDERS o
JOIN store_demo_db.store_demo_sch.ORDER_ITEMS oi ON o.ORDER_ID = oi.ORDER_ID
GROUP BY o.ORDER_ID;

CREATE VIEW store_demo_db.store_demo_sch.V_PRODUCTS_IN_STOCK AS
SELECT 
  p.PRODUCT_ID, 
  p.PRODUCT_NAME, 
  p.STOCK_QUANTITY
FROM store_demo_db.store_demo_sch.PRODUCTS p
WHERE p.STOCK_QUANTITY > 0;

CREATE VIEW store_demo_db.store_demo_sch.V_CUSTOMER_PAYMENT_METHODS AS
SELECT 
  c.CUSTOMER_ID, 
  c.FIRST_NAME, 
  c.LAST_NAME, 
  p.PAYMENT_METHOD
FROM store_demo_db.store_demo_sch.CUSTOMERS c
JOIN PAYMENTS p ON c.CUSTOMER_ID = p.ORDER_ID;

CREATE VIEW store_demo_db.store_demo_sch.V_AVERAGE_ORDER_VALUE AS
SELECT 
  AVG(o.TOTAL) AS AVERAGE_ORDER_VALUE
FROM store_demo_db.store_demo_sch.ORDERS o;

CREATE VIEW store_demo_db.store_demo_sch.V_TOTAL_SALES AS
SELECT 
  SUM(o.TOTAL) AS TOTAL_SALES
FROM store_demo_db.store_demo_sch.ORDERS o;



CREATE VIEW store_demo_db.store_demo_sch.STORE_MANAGER_KPIS AS
SELECT
    -- Total Sales
    (SELECT SUM(TOTAL) FROM store_demo_db.store_demo_sch.ORDERS) AS TOTAL_SALES,
    
    -- Average Order Value
    (SELECT AVG(TOTAL) FROM store_demo_db.store_demo_sch.ORDERS) AS AVERAGE_ORDER_VALUE,
    
    -- Total Orders
    (SELECT COUNT(*) FROM store_demo_db.store_demo_sch.ORDERS) AS TOTAL_ORDERS,
    
    -- Total Customers
    (SELECT COUNT(*) FROM store_demo_db.store_demo_sch.CUSTOMERS) AS TOTAL_CUSTOMERS,
    
    -- Total Products
    (SELECT COUNT(*) FROM store_demo_db.store_demo_sch.PRODUCTS) AS TOTAL_PRODUCTS,
    
    -- Products In Stock
    (SELECT COUNT(*) FROM store_demo_db.store_demo_sch.PRODUCTS WHERE STOCK_QUANTITY > 0) AS PRODUCTS_IN_STOCK,
    
    -- Top Selling Product
    (SELECT PRODUCT_NAME FROM (
        SELECT p.PRODUCT_NAME, SUM(oi.QUANTITY) AS TOTAL_QUANTITY
        FROM ORDER_ITEMS oi
        JOIN store_demo_db.store_demo_sch.PRODUCTS p ON oi.PRODUCT_ID = p.PRODUCT_ID
        GROUP BY p.PRODUCT_NAME
        ORDER BY TOTAL_QUANTITY DESC
    ) WHERE ROWNUM = 1) AS TOP_SELLING_PRODUCT,
    
    -- Total Customer Purchases
    (SELECT SUM(TOTAL_PURCHASES) FROM store_demo_db.store_demo_sch.V_CUSTOMER_PURCHASES) AS TOTAL_CUSTOMER_PURCHASES,
    
    -- Average Customer Purchase
    (SELECT AVG(TOTAL_PURCHASES) FROM store_demo_db.store_demo_sch.V_CUSTOMER_PURCHASES) AS AVERAGE_CUSTOMER_PURCHASE,
    
    -- Total Payments Received
    (SELECT SUM(AMOUNT) FROM store_demo_db.store_demo_sch.PAYMENTS) AS TOTAL_PAYMENTS_RECEIVED,
    
    -- Average Payment Per Order
    (SELECT AVG(AMOUNT) FROM store_demo_db.store_demo_sch.PAYMENTS) AS AVERAGE_PAYMENT_PER_ORDER,
    
    -- Total Inventory Quantity
    (SELECT SUM(QUANTITY) FROM store_demo_db.store_demo_sch.INVENTORY) AS TOTAL_INVENTORY_QUANTITY,
    
    -- Top Customer by Total Purchases
    (SELECT FIRST_NAME || ' ' || LAST_NAME FROM (
        SELECT c.FIRST_NAME, c.LAST_NAME, SUM(o.TOTAL) AS TOTAL_PURCHASES
        FROM store_demo_db.store_demo_sch.CUSTOMERS c
        JOIN store_demo_db.store_demo_sch.ORDERS o ON c.CUSTOMER_ID = o.CUSTOMER_ID
        GROUP BY c.FIRST_NAME, c.LAST_NAME
        ORDER BY TOTAL_PURCHASES DESC
    ) WHERE ROWNUM = 1) AS TOP_CUSTOMER
FROM dual;
