(:returns all of the orderIDs of one customer :)
for $j in 
doc("Payment.xml")/Payments/Payment

where contains($j/@Payment.ID, "22346284")
return
        <orders_id>
        {string($j/Payment.orderID)}
        </orders_id>