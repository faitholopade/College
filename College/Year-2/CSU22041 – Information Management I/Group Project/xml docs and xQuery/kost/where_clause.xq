for $j in 
doc("ConsumerAccount.xml")/ConsumerAccounts/ConsumerAccount

where contains($j/@Consumer.IDNumber, "145645567")
return
        <consumer_Name>
        {string($j/Consumer.Name)}
        </consumer_Name>