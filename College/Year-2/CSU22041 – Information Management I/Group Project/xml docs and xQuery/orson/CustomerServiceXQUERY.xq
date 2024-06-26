declare function local:checkStatus($input as xs:string+) 
as xs:boolean+
{
  let $firstOutput as xs:boolean? := (contains($input[0], 'true'))
  let $secondOutput as xs:boolean? := (contains($input[1], 'true'))
  let $combinedOutput as xs:boolean+ := ($firstOutput, $secondOutput)
  return $combinedOutput
  
};

local:checkStatus(//CustomerServices/CustomerService/CustomerService.status/string())
 
(:This user defined function (checkStatus) & built in function (contains()) is to be used by the customer Service agent in the use case 'Monitors Deliveries and Payment to quickly check both of our instance orders and see if there have been any issues raised' :)