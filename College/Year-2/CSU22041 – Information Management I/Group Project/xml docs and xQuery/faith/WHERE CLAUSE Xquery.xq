for $w in doc("Driver.xml")/Drivers/Driver, $x in doc("Order.xml")/Orders/Order
where $w/orderDelivered = $x/@id
return
<orderDeliveredbyDriver>
{$w/Driver.driverID}
{$w/Driver.phoneNumber}
{$w/orderDelivered}
</orderDeliveredbyDriver>