for $j in doc("restaurantAccount.xml")/RestaurantAccounts/RestaurantAccount/RestaurantAccount.customerInfo
return
("Customer Info :",string($j))