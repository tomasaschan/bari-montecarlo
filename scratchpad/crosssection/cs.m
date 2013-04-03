
function c = cs(e, kl, kq)
    e0 = 70;

    linpart = (e >= e0).*(kl(1) + kl(2)*e);
    quadpart = (e < e0).*(kq(1) + kq(2)*(e) + kq(3)*(e.^2) + kq(4)*(e.^3) + kq(5)*(e.^4));
    c = linpart+quadpart;
end
