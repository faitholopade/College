declare function local:print_with_discount($discount as xs:decimal?) 
   {
    for $x in doc("restaurantMenu.xml")/RestaurantsMenus/RestaurantMenu/MenuItem
    return
    <print> 
      {$x/dishName}
      {$x/dishPrice * (1 - $discount)}
      {$x/dishRating}
      {$x/dishDetails}
      {string-join($x/dishAllergens, ", ")}
    </print>
   };
   
let $s := doc("restaurantMenu.xml")/RestaurantsMenus/RestaurantMenu/MenuItem
return
<all>
  {"PROMOTION! Place your first order and get 10% off! "}
  {"Items on Menu: "}
  {count($s)}
  {local:print_with_discount(0.1)}
</all>



