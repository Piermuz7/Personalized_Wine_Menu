% countries
country(france).
country(german).
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

% grape regions
grape_from(andalusia, palomino).
grape_from(anjou-saumur, cremant).
grape_from(anjou-saumur, riesling).
grape_from(aragona, carignan).
grape_from(bordeaux, cabernet_franc).
grape_from(bordeaux, cabernet_sauvignon).
grape_from(bordeaux, chardonnay).
grape_from(bordeaux, merlot).
grape_from(burgundy, chardonnay).
grape_from(burgundy, chardonnay_rose).
grape_from(chateau_d_esclans, vermentino).
grape_from(friuli, cabernet_sauvignon).
grape_from(friuli, merlot).
grape_from(piemonte, moscato).
grape_from(rheinhessen, riesling).
grape_from(rhone, marsanne).
grape_from(tuscany, brunello).
grape_from(tuscany, sangiovese).
grape_from(vaucluse, grenache).
grape_from(vaucluse, syrah).
grape_from(vosne_romanee, pinot_noir).
grape_from(abruzzi, trebbiano).

% wine categories
wine_category(bold_red).
wine_category(medium_red).
wine_category(light_red).
wine_category(rose).
wine_category(rich_white).
wine_category(light_white).
wine_category(sparkling).
wine_category(sweet).
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
grape_of(aragone, carignan).
grape_of(brunello_di_montalcino, brunello).
grape_of(brunello_di_montalcino, sangiovese).
grape_of(case_basse_sangiovese, sangiovese).
grape_of(corton_charlemagne_grand_cru, chardonnay).
grape_of(coteau_de_la_beylesse, marsanne).
grape_of(cremant_d_alsace, cremant).
grape_of(fino, palomino).
grape_of(g_max_reisling, riesling).
grape_of(garrus_rose, vermentino).
grape_of(hermitage_aoc, marsanne).
grape_of(juline_chateauneuf_du_pape, grenache).
grape_of(juline_chateauneuf_du_pape, syrah).
grape_of(marsannay, chardonnay_rose).
grape_of(occhio_di_pernice, sangiovese).
grape_of(occhio_di_pernice, syrah).
grape_of(pomerol_chateau_de_salesl, cabernet_franc).
grape_of(pomerol_chateau_de_salesl, cabernet_sauvignon).
grape_of(pomerol_chateau_de_salesl, merlot).
grape_of(romanee_saint, pinot_noir).
grape_of(sauternes, chardonnay).
grape_of(trebbiano_d_abruzzo, trebbiano).
grape_of(vajra_mosacto_d_asti, moscato).

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
meal(grilled_bass_with_herbs).
meal(pulled_pork).
meal(pumpkin_risotto).
meal(roast_chicken_with_herbs).
meal(sliced_beef).

% ideal wines
ideal_wine(grilled_bass_with_herbs, light_white).
ideal_wine(grilled_bass_with_herbs, sparkling).
ideal_wine(pulled_pork, bold_red).
ideal_wine(pulled_pork, medium_red).
ideal_wine(pumpkin_risotto, rich_white).
ideal_wine(pumpkin_risotto, rose).
ideal_wine(roast_chicken_with_herbs, light_red).
ideal_wine(roast_chicken_with_herbs, medium_red).
ideal_wine(roast_chicken_with_herbs, rose).
ideal_wine(sliced_beef, bold_red).
ideal_wine(sliced_beef, medium_red).

% predicates
is_ideal_wine(W,M) :- ideal_wine(M,CAT), category_of(W,CAT), wine_category(CAT).
region_belongs(R,C) :- country(C), region(R), region_of(C,R).
grape_origin(G,W,R) :- grape_from(R,G), grape_of(W,G), grape(G).

suggested_wines(W,M,C,R,G,T,TL,D) :- 
    wine(W),
    meal(M), 
    is_ideal_wine(W,M),
    region_belongs(R,C),
    grape_origin(G,W,R),
    taste_of(W,T),
    tannin_of(W,TL),
    dryness_of(W,D).
