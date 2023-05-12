let $x := doc("restaurantMenu.xml")/RestaurantsMenus/RestaurantMenu/MenuItem
return
<total>
  {"Your total is: "}
  {sum($x/dishPrice/text())}
  {" EUR"}
</total>
