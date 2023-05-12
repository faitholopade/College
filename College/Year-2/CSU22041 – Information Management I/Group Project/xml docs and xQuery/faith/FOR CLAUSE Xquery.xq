for $j in 
doc("Order.xml")/Orders/Order/Order.orderDetails
return
("Order Details Node:", $j)