from_book

plots = 1;
from_web



plot([all_eq all_el], exp(K(1)+K(2)*log([all_eq all_el])+K(3)*log([all_eq all_el]).^2)*1e-4, 'm')
