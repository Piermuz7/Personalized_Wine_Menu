# Personalized_Wine_Menu
Personalized_Wine_Menu project developed for KNOWLEDGE ENGINEERING AND BUSINESS INTELLIGENCE exam [ST0906] in the academic year 2022-2023 at University of Camerino.

## Description:
With COVID-19 many restaurants have their menus digitized. Guests can scan a QR code and have the menu presented on their smartphones. A disadvantage is that the screen is very small, and it is difficult to get an overview. This is especially true if a restaurant offers a big wine menu.
Usually customers have preferences. Some prefer wine only from a specific country, e.g. Italy, or a specific region[^1] like the Lombardy. Others exclude or prefer specific grapes[^2], e.g. some don't like Pinot Noir. (Please note that some wines are made from several grapes.) Most people are not wine experts and would like to select their wine by describing the taste (i.e. dry/not-dry, tannin/less-tannin)
But a very prominent decision influencer is the meal[^3]. Red wine usually is offered to meat dishes; white wine usually to fish. But there are exceptions[^4], e.g. white wine with chicken.
The objective of the project is to represent the knowledge about wines. Menus and guest preferences are needed to support the selection process (i.e. they are input). Create a system that allows to select those wines that fit the guest preferences and the menu.
The knowledge base shall contain information about typical wines (of an international restaurant) with wines from different regions and countries. For the taste, the grapes and the meals focus on five major representatives (of your choice).

## Tasks

The following tasks needs to be done:
1. Define input and output of the knowledge-based system. Please consider these
additional constraints for the inputs:
* Because we are selecting wines for a meal from the menu, the meal is input. For example "Spaghetti Alla Carbonara", "Risotto Alla Milanese", or "Cheese Fondue". Of course, a meal then consists of beef, chicken, white fish etc.
 Prevent that the user can select not correlating countries and regions, i.e. it should not be possible (or noted as an error) to select Lombardy and US.
* Some wines are made from several grapes.
   EVERY input might be negated by the user, not only the grapes.
  <br>
Further consider these constraints for the outputs:
  * Of course, the output is a list of concrete wines (in Prolog, you might return the wines one after another).
  * For each wine a matching factor is provided which describes how good the
wine matches the meal. There is a perfect match and a match (cmp. footnote [^4])
2. Create different knowledge-based solutions based on 
*   decision tables
*   prolog
* knowledge graph.
3. Design a graphical modeling language, which allows a chef to represent meals and wines in a graphical way, such that it contains all information relevant for the customers to select according to their preferences
Write a brief explanation of each solution and a conclusion chapter that explains the advantages and disadvantages of the three knowledge-based solutions.

[^1]: https://en.wikipedia.org/wiki/List_of_wine-producing_regions
[^2]: https://en.wikipedia.org/wiki/List_of_grape_varieties
[^3]: https://winefolly.com/wine-pairing/getting-started-with-food-and-wine-pairing/
[^4]: https://media.winefolly.com/food-and-wine-poster.jpg
