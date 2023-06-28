% list operation (utility for grape inclusion/exclusion)
is_in_list(H,[H|_]).
is_in_list(H,[_|T]) :- is_in_list(H,T).
list_intersection([], _, []).
list_intersection([H|T], L2, [H|L1]) :- is_in_list(H,L2), !, list_intersection(T,L2,L1).
list_intersection([_|T],L2,L1) :- list_intersection(T,L2,L1).

% countries
country(france).
country(germany).
country(italy).
country(spain).

% regions
region(abruzzi).
region(anjou-saumur).
region(aragona).
region(andalusia).
region(bordeaux).
region(burgundy).
region(chateau_d_esclans).
region(piemonte).
region(rheinhessen).
region(rhone).
region(tuscany).
region(vaucluse).
region(vosne_romanee).

% country regions
region_of(france, anjou-saumur).
region_of(france, bordeaux).
region_of(france, burgundy).
region_of(france, chateau_d_esclans).
region_of(france, rhone).
region_of(france, vaucluse).
region_of(france, vosne_romanee).
region_of(germany, rheinhessen).
region_of(italy, abruzzi).
region_of(italy, friuli).
region_of(italy, piemonte).
region_of(italy, tuscany).
region_of(spain, andalusia).
region_of(spain, aragona).

% grapes
grape(brunello).
grape(cabernet_franc).
grape(cabernet_sauvignon).
grape(carignan).
grape(chardonnay).
grape(chardonnay_rose).
grape(cremant).
grape(grenache).
grape(marsanne).
grape(merlot).
grape(moscato).
grape(palomino).
grape(pinot_noir).
grape(riesling).
grape(sangiovese).
grape(syrah).
grape(trebbiano).
grape(vermentino).

% wine categories
wine_category(bold_red).
wine_category(medium_red).
wine_category(light_red).
wine_category(rose).
wine_category(rich_white).
wine_category(light_white).
wine_category(sparkling).
wine_category(sweet_white).
wine_category(dessert).

% tastes
taste(bold).
taste(light).

% tannin levels
tannin_level(tannic).
tannin_level(less_tannic).

% dryness
dry_level(dry).
dry_level(not_dry).

% wines
wine(aragone).
wine(brunello_di_montalcino).
wine(case_basse_sangiovese).
wine(corton_charlemagne_grand_cru).
wine(coteau_de_la_beylesse).
wine(cremant_d_alsace).
wine(fino).
wine(g_max_reisling).
wine(garrus_rose).
wine(hermitage_aoc).
wine(juline_chateauneuf_du_pape).
wine(marsannay).
wine(occhio_di_pernice).
wine(pomerol_chateau_de_salesl).
wine(romanee_saint).
wine(sauternes).
wine(trebbiano_d_abruzzo).
wine(vajra_mosacto_d_asti).

% wine grapes
grapes_of(aragone, [carignan]).
grapes_of(brunello_di_montalcino, [brunello,sangiovese]).
grapes_of(case_basse_sangiovese, [sangiovese]).
grapes_of(corton_charlemagne_grand_cru, [chardonnay]).
grapes_of(coteau_de_la_beylesse, [marsanne]).
grapes_of(cremant_d_alsace, [cremant]).
grapes_of(fino, [palomino]).
grapes_of(g_max_reisling, [riesling]).
grapes_of(garrus_rose, [vermentino]).
grapes_of(hermitage_aoc, [marsanne]).
grapes_of(juline_chateauneuf_du_pape, [grenache,syrah]).
grapes_of(marsannay, [chardonnay_rose]).
grapes_of(occhio_di_pernice, [sangiovese,syrah]).
grapes_of(pomerol_chateau_de_salesl, [cabernet_franc,cabernet_sauvignon,merlot]).
grapes_of(romanee_saint, [pinot_noir]).
grapes_of(sauternes, [chardonnay]).
grapes_of(trebbiano_d_abruzzo, [trebbiano]).
grapes_of(vajra_mosacto_d_asti, [moscato]).

% wine regions
wine_from(aragona, aragone).
wine_from(tuscany, brunello_di_montalcino).
wine_from(tuscany, case_basse_sangiovese).
wine_from(burgundy, corton_charlemagne_grand_cru).
wine_from(rhone, coteau_de_la_beylesse).
wine_from(anjou-saumur, cremant_d_alsace).
wine_from(andalusia, fino).
wine_from(rheinhessen, g_max_reisling).
wine_from(chateau_d_esclans, garrus_rose).
wine_from(rhone, hermitage_aoc).
wine_from(vaucluse, juline_chateauneuf_du_pape).
wine_from(burgundy, marsannay).
wine_from(tuscany, occhio_di_pernice).
wine_from(bordeaux, pomerol_chateau_de_salesl).
wine_from(vosne_romanee,romanee_saint).
wine_from(bordeaux, sauternes).
wine_from(abruzzi, trebbiano_d_abruzzo).
wine_from(piemonte, vajra_mosacto_d_asti).

% wine categories
category_of(brunello_di_montalcino, bold_red).
category_of(juline_chateauneuf_du_pape, bold_red).
category_of(case_basse_sangiovese, medium_red).
category_of(pomerol_chateau_de_salesl, medium_red).
category_of(aragone, light_red).
category_of(romanee_saint, light_red).
category_of(garrus_rose, rose).
category_of(marsannay, rose).
category_of(corton_charlemagne_grand_cru, rich_white).
category_of(hermitage_aoc, rich_white).
category_of(sauternes, light_white).
category_of(trebbiano_d_abruzzo, light_white).
category_of(coteau_de_la_beylesse, sparkling).
category_of(cremant_d_alsace, sparkling).
category_of(g_max_reisling, sweet_white).
category_of(vajra_mosacto_d_asti, sweet_white).
category_of(fino, dessert).
category_of(occhio_di_pernice, dessert).

% wine tastes
taste_of(aragone, bold).
taste_of(brunello_di_montalcino, bold).
taste_of(case_basse_sangiovese, bold).
taste_of(corton_charlemagne_grand_cru, light).
taste_of(coteau_de_la_beylesse, bold).
taste_of(cremant_d_alsace, bold).
taste_of(fino, bold).
taste_of(g_max_reisling, bold).
taste_of(garrus_rose, light).
taste_of(hermitage_aoc, bold).
taste_of(juline_chateauneuf_du_pape, bold).
taste_of(marsannay, light).
taste_of(occhio_di_pernice, light).
taste_of(pomerol_chateau_de_salesl, bold).
taste_of(romanee_saint, bold).
taste_of(sauternes, bold).
taste_of(trebbiano_d_abruzzo, bold).
taste_of(vajra_mosacto_d_asti, light).

% wine tannin levels
tannin_of(aragone, tannic).
tannin_of(brunello_di_montalcino, tannic).
tannin_of(case_basse_sangiovese, tannic).
tannin_of(corton_charlemagne_grand_cru, tannic).
tannin_of(coteau_de_la_beylesse, tannic).
tannin_of(cremant_d_alsace, less_tannic).
tannin_of(fino, less_tannic).
tannin_of(g_max_reisling, less_tannic).
tannin_of(garrus_rose, tannic).
tannin_of(hermitage_aoc, tannic).
tannin_of(juline_chateauneuf_du_pape, tannic).
tannin_of(marsannay, tannic).
tannin_of(occhio_di_pernice, tannic).
tannin_of(pomerol_chateau_de_salesl, tannic).
tannin_of(romanee_saint, tannic).
tannin_of(sauternes, less_tannic).
tannin_of(trebbiano_d_abruzzo, less_tannic).
tannin_of(vajra_mosacto_d_asti, less_tannic).

% wine drynesses
dryness_of(aragone, dry).
dryness_of(brunello_di_montalcino, dry).
dryness_of(case_basse_sangiovese, dry).
dryness_of(corton_charlemagne_grand_cru, dry).
dryness_of(coteau_de_la_beylesse, dry).
dryness_of(cremant_d_alsace, dry).
dryness_of(fino, dry).
dryness_of(g_max_reisling, dry).
dryness_of(garrus_rose, dry).
dryness_of(hermitage_aoc, dry).
dryness_of(juline_chateauneuf_du_pape, dry).
dryness_of(marsannay, not_dry).
dryness_of(occhio_di_pernice, not_dry).
dryness_of(pomerol_chateau_de_salesl, dry).
dryness_of(romanee_saint, dry).
dryness_of(sauternes, not_dry).
dryness_of(trebbiano_d_abruzzo, dry).
dryness_of(vajra_mosacto_d_asti, dry).

% meals
meal(fried_bass_with_herbs).
meal(pulled_pork).
meal(pumpkin_risotto).
meal(roast_chicken_with_herbs).
meal(sliced_beef).
meal(strawberry_cheesecake).

% ideal strong wines
ideal_strong_wine(fried_bass_with_herbs, light_white).
ideal_strong_wine(pulled_pork, bold_red).
ideal_strong_wine(pulled_pork, medium_red).
ideal_strong_wine(pumpkin_risotto, rich_white).
ideal_strong_wine(pumpkin_risotto, rose).
ideal_strong_wine(roast_chicken_with_herbs, light_red).
ideal_strong_wine(sliced_beef, bold_red).
ideal_strong_wine(strawberry_cheesecake, sweet_white).

% ideal weak wines
ideal_weak_wine(fried_bass_with_herbs, rich_white).
ideal_weak_wine(pumpkin_risotto, sweet_white).
ideal_weak_wine(roast_chicken_with_herbs, medium_red).
ideal_weak_wine(roast_chicken_with_herbs, rose).
ideal_weak_wine(sliced_beef, medium_red).
ideal_weak_wine(strawberry_cheesecake, dessert).
ideal_weak_wine(strawberry_cheesecake, sparkling).

% predicates
wine_checking(W,C,R,T,TL,D) :- wine(W), 
    region_belongs(R,C), wine_origin(W,R), taste_of(W,T), 
    tannin_of(W,TL), dryness_of(W,D).

is_ideal_strong_wine(W,M) :- ideal_strong_wine(M,CAT), category_of(W,CAT), wine_category(CAT).

is_ideal_weak_wine(W,M) :- ideal_weak_wine(M,CAT), category_of(W,CAT), wine_category(CAT).

region_belongs(R,C) :- country(C), region(R), region_of(C,R).

wine_origin(W,R) :- wine_from(R,W).

include_wine_by_grape(W,IG) :- IG=[] -> (grapes_of(W,_));
    grapes_of(W,GS),is_in_list(G,IG), is_in_list(G,GS).

exclude_wine_by_grape(W,EG) :- grapes_of(W,GS), list_intersection(GS,EG,NW), 
    not(is_in_list(G,NW)), is_in_list(G,GS).

suggested_strong_wines(STRONG,M,IG,EG,C,R,T,TL,D) :-
	wine_checking(STRONG,C,R,T,TL,D),
    include_wine_by_grape(STRONG,IG),
    exclude_wine_by_grape(STRONG,EG),
    meal(M),
    is_ideal_strong_wine(STRONG,M).

suggested_weak_wines(WEAK,M,IG,EG,C,R,T,TL,D) :-
	wine_checking(WEAK,C,R,T,TL,D),
    include_wine_by_grape(WEAK,IG),
	exclude_wine_by_grape(WEAK,EG),
    meal(M),
    is_ideal_weak_wine(WEAK,M).

% suggested_wines(STRONG,WEAK,roast_chicken_with_herbs,[],[],C,R,T,TL,D)
% NOTE: M, the meal is needed.
% IG, Include Grape and EG, Exclude Grape should be empty lists [], [] if
% there are no preferences about inclusion and exclusion of grapes.

suggested_wines(STRONG,WEAK,M,IG,EG,C,R,T,TL,D) :-
    suggested_strong_wines(STRONG,M,IG,EG,C,R,T,TL,D);
    suggested_weak_wines(WEAK,M,IG,EG,C,R,T,TL,D).
