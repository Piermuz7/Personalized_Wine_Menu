% list operations
% * list membership (utility for grape inclusion/exclusion)
is_in_list(H,[H|_]).
is_in_list(H,[_|T]) :- is_in_list(H,T).
list_intersection([], _, []).
list_intersection([H|T], L2, [H|L1]) :- is_in_list(H,L2), !, list_intersection(T,L2,L1).
list_intersection([_|T],L2,L1) :- list_intersection(T,L2,L1).
% * concatenation between lists including duplicates (utility for meal derivation from ingredients)
list_concat([],L,L).
list_concat([X1|L1],L2,[X1|L3]) :- list_concat(L1,L2,L3).
% * count list occurrencies (utility for meal derivation from ingredients)
list_count([],_H,0). %_H to suppress Singleton Warning
list_count([H|T],H,C):- list_count(T,H,O), C is 1+O.
list_count([H1|T],H,C):- H1\=H, list_count(T,H,C).
list_countall(L,H,C) :-
    is_in_list(H,L),
    list_count(L,H,C).

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
grapes_of(brunello_di_montalcino, [brunello, sangiovese]).
grapes_of(case_basse_sangiovese, [sangiovese]).
grapes_of(corton_charlemagne_grand_cru, [chardonnay]).
grapes_of(coteau_de_la_beylesse, [marsanne]).
grapes_of(cremant_d_alsace, [cremant]).
grapes_of(fino, [palomino]).
grapes_of(g_max_reisling, [riesling]).
grapes_of(garrus_rose, [vermentino]).
grapes_of(hermitage_aoc, [marsanne]).
grapes_of(juline_chateauneuf_du_pape, [grenache, syrah]).
grapes_of(marsannay, [chardonnay_rose]).
grapes_of(occhio_di_pernice, [sangiovese, syrah]).
grapes_of(pomerol_chateau_de_salesl, [cabernet_franc, cabernet_sauvignon, merlot]).
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
wine_from(vosne_romanee, romanee_saint).
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

% ingredients
ingredient(fish).
ingredient(herbs).
ingredient(pork).
ingredient(saulted_or_fried).
ingredient(black_pepper).
ingredient(white_starches).
ingredient(alliums).
ingredient(red_pepper).
ingredient(grilled).
ingredient(smoked).
ingredient(red_meat).
ingredient(root_vegetables_and_squash).
ingredient(soft_cheese_and_cream).
ingredient(poultry).
ingredient(roasted).
ingredient(fruit_and_barries).

% meals
meal(fried_bass_with_herbs).
meal(pulled_pork).
meal(pumpkin_risotto).
meal(roast_chicken_with_herbs).
meal(sliced_beef).
meal(strawberry_cheesecake).
meal(pumpkin_risotto).

% meal ingredients
ingredient_of(fried_bass_with_herbs, [fish, herbs, saulted_or_fried]).
ingredient_of(pulled_pork, [pork, black_pepper, white_starches, alliums, red_pepper, grilled, smoked]).
ingredient_of(sliced_beef, [red_meat, grilled]).
ingredient_of(pumpkin_risotto, [white_starches, root_vegetables_and_squash, soft_cheese_and_cream]).
ingredient_of(roast_chicken_with_herbs, [poultry, herbs, roasted]).
ingredient_of(strawberry_cheesecake, [fruit_and_barries]).

% ingredients strong categories
strong_category_of(fish, light_white).
strong_category_of(herbs, light_white).
strong_category_of(saulted_or_fried, light_red).
strong_category_of(black_pepper, bold_red).
strong_category_of(pork, medium_red).
strong_category_of(red_pepper, medium_red).
strong_category_of(alliums, medium_red).
strong_category_of(grilled, bold_red).
strong_category_of(smoked, medium_red).
strong_category_of(red_meat, bold_red).
strong_category_of(root_vegetables_and_squash, rose).
strong_category_of(soft_cheese_and_cream, light_red).
strong_category_of(soft_cheese_and_cream, rich_white).
strong_category_of(poultry, light_red).
strong_category_of(poultry, rich_white).
strong_category_of(roasted, bold_red).
strong_category_of(fruit_and_barries, sweet_white).

% ingredients weak categories
weak_category_of(fish, rich_white).
weak_category_of(fish, sparkling).
weak_category_of(herbs, rich_white).
weak_category_of(herbs, rose).
weak_category_of(herbs, light_red).
weak_category_of(saulted_or_fried, rose).
weak_category_of(saulted_or_fried, rich_white).
weak_category_of(saulted_or_fried, light_white).
weak_category_of(saulted_or_fried, sparkling).
weak_category_of(pork, bold_red).
weak_category_of(pork, rose).
weak_category_of(pork, sparkling).
weak_category_of(black_pepper, medium_red).
weak_category_of(white_starches, bold_red).
weak_category_of(white_starches, medium_red).
weak_category_of(white_starches, light_red).
weak_category_of(white_starches, rose).
weak_category_of(white_starches, rich_white).
weak_category_of(white_starches, light_white).
weak_category_of(white_starches, sparkling).
weak_category_of(white_starches, sweet_white).
weak_category_of(white_starches, dessert).
weak_category_of(alliums, bold_red).
weak_category_of(alliums, light_red).
weak_category_of(alliums, rose).
weak_category_of(alliums, rich_white).
weak_category_of(alliums, light_white).
weak_category_of(alliums, sparkling).
weak_category_of(alliums, sweet_white).
weak_category_of(red_pepper, bold_red).
weak_category_of(red_pepper, rose).
weak_category_of(red_pepper, light_white).
weak_category_of(red_pepper, sparkling).
weak_category_of(red_pepper, sweet_white).
weak_category_of(grilled, medium_red).
weak_category_of(grilled, light_red).
weak_category_of(grilled, sparkling).
weak_category_of(grilled, sweet_white).
weak_category_of(smoked, bold_red).
weak_category_of(smoked, light_red).
weak_category_of(smoked, rose).
weak_category_of(smoked, sparkling).
weak_category_of(smoked, dessert).
weak_category_of(red_meat, medium_red).
weak_category_of(root_vegetables_and_squash, rich_white).
weak_category_of(root_vegetables_and_squash, sweet_white).
weak_category_of(soft_cheese_and_cream, medium_red).
weak_category_of(soft_cheese_and_cream, rose).
weak_category_of(soft_cheese_and_cream, light_white).
weak_category_of(soft_cheese_and_cream, sparkling).
weak_category_of(soft_cheese_and_cream, dessert).
weak_category_of(soft_cheese_and_cream, sweet_white).
weak_category_of(poultry, medium_red).
weak_category_of(poultry, rose).
weak_category_of(poultry, light_white).
weak_category_of(poultry, sparkling).
weak_category_of(poultry, medium_red).
weak_category_of(poultry, light_red).
weak_category_of(poultry, rose).
weak_category_of(roasted, medium_red).
weak_category_of(roasted, light_red).
weak_category_of(roasted, rose).
weak_category_of(fruit_and_barries, sparkling).
weak_category_of(fruit_and_barries, dessert).

% rules
wine_checking(W,C,R,T,TL,D) :- wine(W), 
    region_belongs(R,C), wine_from(R,W), taste_of(W,T), 
    tannin_of(W,TL), dryness_of(W,D).

ideal_strong_category(M,C) :-
    ingredient_of(M,I),
    is_in_list(ING,I),
    strong_category_of(ING,C).

ideal_weak_category(M,C) :-
    ingredient_of(M,I),
    is_in_list(ING,I),
    weak_category_of(ING,C).

get_strongs(M,R) :- bagof(
               X, 
               ideal_strong_category(M,X),
               R).

get_weaks(M,R) :- bagof(
               X,
               ideal_weak_category(M,X),
               R).

ideal_strong_categories(M,C) :- 
    setof(CAT,
          (get_strongs(M,R1),
		  get_weaks(M,R2),
		  list_concat(R1,R2,R),
              wine_category(CAT),
              is_in_list(CAT,R1), % check if is a strong category
              is_in_list(CAT,R), % and is in concatenation list
              list_countall(R,CAT,O),
              ingredient_of(M,I),
              length(I,L), O >= L),
          C).

ideal_weak_categories(M,C) :- 
    setof(CAT, 
          (bagof(
               X,
               ideal_weak_category(M,X),
               R),
              wine_category(CAT), is_in_list(CAT,R),
              list_countall(R,CAT,L),
              ingredient_of(M,I),
              length(I,L)),
          C).

is_ideal_strong_wine(W,M) :-  wine_category(CAT), ideal_strong_categories(M,C),
    is_in_list(CAT,C), category_of(W,CAT).

is_ideal_weak_wine(W,M) :- wine_category(CAT), ideal_weak_categories(M,C),
    is_in_list(CAT,C), category_of(W,CAT).

region_belongs(R,C) :- country(C), region(R), region_of(C,R).

include_wine_by_grape(W,IG) :- IG=[] -> (grapes_of(W,_));
    grapes_of(W,GS),is_in_list(G,IG), is_in_list(G,GS).

exclude_wine_by_grape(W,EG) :- grapes_of(W,GS), list_intersection(GS,EG,NW), 
    not(is_in_list(G,NW)), is_in_list(G,GS).

suggested_general_wines(W,M,IG,EG,C,R,T,TL,D) :-
    wine_checking(W,C,R,T,TL,D),
    include_wine_by_grape(W,IG),
    exclude_wine_by_grape(W,EG),
    meal(M).

suggested_strong_wines(STRONG,M,IG,EG,C,R,T,TL,D) :-
	suggested_general_wines(STRONG,M,IG,EG,C,R,T,TL,D),
    is_ideal_strong_wine(STRONG,M).

suggested_weak_wines(WEAK,M,IG,EG,C,R,T,TL,D) :-
	suggested_general_wines(WEAK,M,IG,EG,C,R,T,TL,D),
    is_ideal_weak_wine(WEAK,M).

% suggested_wines(STRONG,WEAK,roast_chicken_with_herbs,[],[],C,R,T,TL,D)
% NOTE: M, the meal is needed.
% IG, Include Grape and EG, Exclude Grape should be empty lists [], [] if
% there are no preferences about inclusion and exclusion of grapes.

suggested_wines(STRONG,WEAK,M,IG,EG,C,R,T,TL,D) :-
    setof(STRONG, suggested_strong_wines(STRONG,M,IG,EG,C,R,T,TL,D), STRONG);
    setof(WEAK, suggested_weak_wines(WEAK,M,IG,EG,C,R,T,TL,D), WEAK).
