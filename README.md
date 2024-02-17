# FISHCODE
Agent-based model for Ger fisheries in the southern North Sea

WHAT IS IT?

This Agent-based model (ABM) simulates the spatio-temporal dynamics of German fishers in the southern North Sea by applying high temporal (daily) and spatial (0.045 lon x 0.045 lat) resolution and a complex human decision-making methodology called the Consumat approach that goes beyond pure profit maximization. The aim of the ABM is to test how different scenarios affect the spatio-temporal behavior and adaptive capacity of the fishers. Scenarios will encompass changes in resource availability (e.g. species migrations due to climate change), closed fishing area (e.g. more OWFs or MPAs), and market prices. These scenarios provide insights into potential future fishing effort displacement and shifting target species and fishing gears, which is useful information for effective fisheries management. Moreover, the ABM assesss the applicability of the Consumat approach for fishers’ behavior beyond rational decision-making. By building on existing behavioral theories for fishers, we use the ABM to anaylze the behavioral motivations of three German fleets, namely the near-coastal beam trawl fleet catching shrimps, the beam trawl fleet catching the flatfishes sole & plaice, and the otter board fleet catching plaice & Nephrops

HOW IT WORKS

Every agent (fishing vessel) decides daily whether to go fishing (unless its already on a fishing trip), and in what fishing metiér to engage. Metiérs are a combination of gear and target species and depending on the technical characteristics and quota availabilities, every vessel has a different pool of potential metiérs to choose from. External factors, such as wave height, fish prices and fuel prices influence the agents’ decisions. One sea, agents prefer areas with less fishing vessels. We included a complex human decision-making framework, namely the Consumat approach, in which agents’ behavioural strategy is determined by their levels of satisfaction and uncertainty. Depending on these levels, they will engage in one out of four strategies that vary in complexity and social engagement. In general, satisfied agents engage in simpler strategies (e.g. repeating their last metiér choice), whereas unsatisfied agents choose more complex strategies (e.g. evaluating several metiér options). Uncertain agents engage in more social strategies (e.g. evaluating options of their colleagues) and certain agents in more individual strategies (e.g. repeating their last metiér option). In our model, each agent has three satisfactions (personal, social, and existence) and two uncertainties (social and existence) that each stand for different behavioural elements.

HOW TO USE IT

setup - Initialization. Setting up the model according to files in the model_input folder.
path_input_folder - file path to the folder with model input files
path_output_folder - file path to where exported results should be saved
one day - One time step/tick (day)
go - Executes ticks until the chosen end date of the model is reached (usually one run = one year)
Scenario? - Choose which scenario should be run
show_ves_distr - visual representation of international fishing vessels
show_ves_path - visual representation of German fishing vessels movements
show_depletion - visual representation of local depletion

THINGS TO NOTICE

The first day takes long, because yearly environmental variables are set up
Depending on the spatial scenario, the fishing effort will be restricted by spatial fishing restrictions (red), i.e. offshore wind farms and marine protected areas
In areas with high fishing effort (e.g. close to the coast), local depletion is higher
Fishing trip data (used to simulated landings) and offshore wind farm data are not publicly available. Therefore, we removed offshore wind farm scenarios, and replaced real fishing trip data with simulated proxy data.
All results including the analyis of scenarios are done in sepperated R scripts. Scenarios are compared to a baseline scenario to assess their effect.

THINGS TO TRY

Choose scenarios and run the model
Play with the model parameters
Implement your own shapefiles for fishing restrictions (code for preparation can be provided)

