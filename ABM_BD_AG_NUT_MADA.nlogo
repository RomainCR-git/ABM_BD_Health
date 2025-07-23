;;;;;;;;;;;;
;; BREEDS ;; ; 'breeds' are the different types of farmers in the model. They are distinguished by their agricultural practices: shifting agriculture, lowland rice monoculture, or diverse agroforest + home-garden
;;;;;;;;;;;;

breed [ tavy-producers tavy-producer ]
breed [ paddy-producers paddy-producer ]
breed [ diverse-producers diverse-producer ]


;;;;;;;;;;;;;;;
;; VARIABLES ;;
;;;;;;;;;;;;;;;

globals [ ; 'globals' are variables at landscape/model scale, i.e. they do not vary by farmer or plot
  energy-needs ; the amount of energy per capita per year that is needed to ensure proper nutrition
  conservation-benefits ; the amount of energy provided as a payement for ecosystem services
;; biodiversity variables
  forest-cover ; the proportion of patches with land use = forest
  vegetation-cover ; the vegetation cover is the mean vegetation cover of all cells, which thus takes into account the forests and partial vegetation cover on fallows and cultivated patches
  forest-biodiversity ; the level of biodiversity in forest, varies non-linearly with forest cover: below a threshold, biodiversity collapses
  edible-species-diversity ; diversity of forest species; this is a global variable rather than a patch variable at this stage
  opportunistic-foraging ; the number of routine foraging events
  purposeful-foraging ; the number of foraging events to avoid food insecurity
  fc_threshold ; the forest cover threshold bellow which biodiversity in forest collapses, varies depending on tree cover in farms
;; agricultural variables
  mean-tree-cover ; mean tree cover of agricultural plots
  land-equivalent-ratio ; a yield multiplier for intercrops (agroforests and home gardens)
  max-S&B-yield ; the highest yield that could be expected with perfect soil fertility and no biotic or abiotic disruption, using slash and burn agriculture
  max-paddy-yield ; highest yield for paddy rice without external input
  max-SRI-yield ; highest yield for paddy rice in SRI with fish and ducks, an average of production from rice and animals on the patch
  max-agroforest-yield ; highest yield for SALT agroforestry, as such an average of the various crops on the patch
  max-homegarden-yield ; highest yield for homegardens, an average
  harvest-loss
;; disruption variables
  biotic-disruption ; a random number representing the percentage of production loss to crop pest pressure each year
  abiotic-disruption ; a random number representing the percentage of production loss to climatic events each year
;; sums over 120 years
]

turtles-own [ ; 'turtles' are farmers
  age ; age of adults in the household -> updated at the beginning of the demographic processes
  children-number ; the n. of children in the household -> updated during the demographic processes
  energy-target ; the amount of energy the household will aim to obtain during this tick -> updated at the end of the demographic processes
  production-target ;
  production ; the annual agricultural production per household
  energy-from-plots ; the energy obtained from harvesting plots during this tick -> updated during the land use processes
  energy-from-forest ; the energy obtained from foraging during this tick -> updated during the land use processes
  energy-from-conservation ; energy obtained from conserving forests
  energy-per-capita ; energy / number of persons in the household -> updated during the farmer outcome process
  food-stock ; energy stored if harvest produce surpluses
  food-groups ; a measure of diet diversity. 10 food groups, based on the MDD-W
  forest-desired ; the amount of forest cover each HH will aim to maintain (by increasing land use intensity rather than expanding over forest)
  cultivated-plots ; the number of plots cultivated by this household -> updated during the farmer outcome process
  fallowed-plots ; the number of plots fallowed by this household -> updated during the farmer outcome process
]

patches-own [ ; patches are land units
  owner ; the identity of the patch owner, which can be nobody
  land-use ; either forest, S&B, paddy, SRI, garden, agroforest, fallow, or degraded
;; biodiversity variables
  soil-biodiversity ; between 0-1 with 1 being the level found in forests
  perennial-cover ; between 0-1 with 1 being the level found in forests
  production-diversity ; the number of crops on a plot / 10 (i.e. 0.1 = 1 crop)
;; ecosystem services variables - only computed for agricultural plots, they are used to compute the yield
  pollination-service ; the percentage of pollination service the patch benefits from
  climate-regulation ; the percentage of climatic disruption that the patch is shielded from
  pest-regulation ; the percentage of pest pressure that the patch is shielded from
  soil-fertility ; the percentage of fertility the patch benefits from
;; agricultural variables - only computed for agricultural plots
  expected-yield ; the yield that can be expected if no disruption occurs, based on current levels of soil fertility and other ecosystem services
  achieved-yield ; the yield that is achieved after disruption
  years-fallowed ; for fallowed plots, how many years they have been left fallowed
  years-cultivated ; how many years the plot has been cultivated since the last fallow
  years-cultivated-total ; the total number of years cultivated
  degradation ; for fallowed and cultivated plots, a measure of their over-use and ability to recover soil biodiversity and vegetation cover
]


;;;;;;;;;;;;;;;;;;;;
;; INITIALISATION ;;
;;;;;;;;;;;;;;;;;;;;

to setup ; creates the model initial conditions
  ca ; clear previous model run
  reset-ticks ; start at year 0
;  random-seed 42 ; some model components are stochastic, this parameter keeps them reproducible between model runs
;; first set global variables values
  set energy-needs 876 ; the yearly energy needs of a household member, in thousand kcal (2400*365 = 876.000 kcal). An adult male with intense activity requires 1277.500
  set forest-cover 1
  set vegetation-cover 1
  set forest-biodiversity 1
  set edible-species-diversity 1
  let rice-milling 0.7 ; the difference between the paddy yield and the polished rice yield. About 20% of the harvested weigth is inedible husk and 10% bran, removed from white rice.
  set max-S&B-yield (1050 * rice-milling * potential-yield-weight); (water limited yield: 700 = 2T, 880 = 2.5T, 1050 = 3T, 1400 = 4T) estimated maximum energy for 0.1ha. 1T/ha * 0.1 * 3500kcal (100g of dry rice will provide ~350 kcal)
  set max-paddy-yield (3150 * rice-milling * potential-yield-weight); 9T/ha
  set max-agroforest-yield 1800 * potential-yield-weight ; +30% = 2340;
  set max-homegarden-yield 560 * potential-yield-weight; +30% = 728;
  set land-equivalent-ratio LER
;; then create a forested landscape
  ask patches [
    set land-use "forest"
    set pcolor 72
    set soil-biodiversity 1
    set perennial-cover 1
    set owner nobody
    set degradation 0
  ]
;; then create farmer households
  create-tavy-producers S&B-households ; create initial slash and burn households
  ask tavy-producers [ set color red ]
  create-paddy-producers paddy-households ; create initial paddy households
  ask paddy-producers  [ set color blue ]
  create-diverse-producers diverse-households ; create initial diverse producers
  ask diverse-producers [ set color green ]
  ask turtles [ ; set location size and shape of turtles
    set shape "person"
    set size 2
    set age 17 + random 17 ; the first timestep will add one year
    set children-number 2 ;
    setxy random-xcor random-ycor
    set forest-desired random-normal desired-forest-cover (desired-forest-cover / 3)
    ]
end


;;;;;;;;;;;;;;;;;;;;;
;; MASTER SCHEDULE ;; ; this determines which processes are executed by the model at each time step, and in which order. The processes are detailed after
;;;;;;;;;;;;;;;;;;;;;

to go
  update-globals
  grow-population
  attribute-conservation-benefits
  plan-permanent-land-use
  plan-shifting-land-use
  harvest
  move-to-plot
  forage
  recover-fallows
  update-farmer-variables
  if clear-patch-if-starving = "yes" [ clear-protected-patch ]
  if ticks >= simulation-duration [ stop ] ; the model stops after a number of years chosen on the user interface
tick
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SPECIFIC PROCESSES BELOW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;
;; GLOBALS UPDATE ;; ; this process updates the values of the global variables
;;;;;;;;;;;;;;;;;;;;

to update-globals ; each time step starts with updating the global variables, which will then be used to compute patch and farmer variables
;; first update forest and vegetation cover
  set forest-cover ( count patches with [land-use = "forest"] / count patches ) ; the percentage of patches with forest land use
  set vegetation-cover mean [perennial-cover] of patches ; the mean percentage of tree cover on all cells (thus including vegetation on fallows and cultivated land)
;; then determine forest biodiversity
  if ticks > 0 [ ; the lines below will update the forest biodiversity variable based on forest cover. It is done after the 1st tick as there are only forest patches before.
   set mean-tree-cover mean [perennial-cover] of patches with [land-use != "forest"]
    let threshold_ceiling forest-BD-loss-threshold ; the forest cover threshold will range between this value and 0.1 point less (e.g. 0.3 - 0.2)
    let perennial_cover_ceiling (forest-BD-loss-threshold - 0.1) ; the threshold will increase when perennial cover of agricultural land decreases below this value.
    ifelse (mean-tree-cover < perennial_cover_ceiling AND forest-cover < forest-BD-loss-threshold) [
      set fc_threshold threshold_ceiling - (mean-tree-cover / (perennial_cover_ceiling * 10) ) ; The forest cover threshold below which forest biodiversity will drop quickly. The threshold depends on tree-cover on non-forest patches
    ][
      set fc_threshold threshold_ceiling - 0.1]
    ifelse forest-cover <= fc_threshold[
      set forest-biodiversity (forest-cover * 0.6 / fc_threshold) ; this determines forest biodiversity if forest cover is below the threshold: quick drop
    ][
      set forest-biodiversity (0.6 + ((forest-cover - fc_threshold) * 0.4 / (1 - fc_threshold))) ; this determines forest biodiversity if forest cover is above the threshold: gentle drop
    ]
  ]
  set opportunistic-foraging 0; resetting the value for each new tick of foraging pressure (i.e. the number of times farmers forage)
  set purposeful-foraging 0
;; then determine climatic and pest pressure for this turn
ifelse stochastic-disruption = "no" [
    ; make disruption deterministic (i.e. does not vary between years)
    set abiotic-disruption deter-disruption-level
    set biotic-disruption deter-disruption-level
  ][
    ; make disruption stochastic (mean yearly value chosen on the UI)
  set abiotic-disruption ( random-exponential disruption-level ) ; the disruption represents the yearly harvest loss to climatic events. It represents a percentage of the potential harvest. It is reproduces an average of 30% harvests lost to abiotic pressure, but most years will see small losses and few years large losses. It is a number from a random exponential distribution with a mean of 60. The mean is 60 and not 30 because the cap implemented below at 100% causes a random re-draw of high values which lowers the mean. Tests over 20.000 runs have shown that this value of 60 reproduces a mean of 30.
  set biotic-disruption  ( random-exponential disruption-level ) ; same for pest pressure
  while [biotic-disruption + abiotic-disruption > 100] [ ; if the total disruption level is above 100%, the largest of the two is redrawn randomly
    ifelse biotic-disruption > abiotic-disruption[
      set biotic-disruption (random-exponential disruption-level)
      ][
      set abiotic-disruption (random-exponential disruption-level)]
    ]
  ]
;  if biotic-disruption > 100 or abiotic-disruption > 100 or (biotic-disruption + abiotic-disruption > 100) [user-message (word "disruption level above 100")] ; used to detect potential bugs
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEMOGRAPHIC PROCESSES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to grow-population
  if ((count turtles * 2 + ( sum [children-number] of turtles)) / 5.073) < max-pop-density [ have-kids ]; the population will grow only if the desired max population has not been reached
end

to have-kids ; a simpler model to generate population growth: rather than die, older household transform into new households. Additionally, the number of children is fixed, avoiding fluctuations in households energy needs
  ask turtles [
    set age age + 1 ; age is updated and then the number of children set accordingly
    if age = 35 [ create-household ]
  ]
;; older households die and transfer land to their children -> in this simpler implementation, the older household simply 'transforms' into his child
   ask turtles [
    if age >= 64 [
      set age 18
    ]
   ]
end

to create-household ; this process creates a new household for children leaving their parents, and for parents to share some land with them
;; a new farmer household is created from parents.
  hatch 1 [
    set age 18
    set forest-desired random-normal desired-forest-cover (desired-forest-cover / 3) ; age and desired forest cover are manually set, other variables values are inherited from the parents
    ;; the greyed code below was used for debugging
    ;; the code below is to ensure that land sharing is implemented properly. It ensures that only 1 child is on the same plot, so that land is not transfered accidentally to the wrong turtle
;    let no_of_turtles_here count turtles-here
;    if no_of_turtles_here > 2 [
;      inspect self ; use to display the turtle
;      user-message (word "Turtle " who " is with another child on the same plot")
;    if no_of_turtles_here < 2 [ ; this checks whether the child is on the same plot as the parent
;        inspect self ; use to display the turtle
;      user-message (word "Turtle " who " is alone on his patch")
;      ]
;    ]
    create-links-from other turtles-here with-max [age]; the child creates a directed link with his parent
  ]
;; this is the process for parents to share part of their land with their child leaving the household
Let plots-to-share count patches with [ owner = myself ] * 0.5 ; First, count the patches to share. parents aim to share equally between them and their children (another option is to share 40% to have equal per capita land at the time when the division is done)
;Let rounded-plots-to-share int plots-to-share ; making sure the number of plots to share is an integer. This means that parents might share a bit less than a fair share
let child one-of out-link-neighbors with-min [age] ; land is shared with the youngest child
;  if child = nobody [ ; used to ensure the process is implemented properly
;    inspect self
;    user-message (word "Turtle " who "has no child")]
ask my-out-links [ die ] ; removes the link between the parents and child. Necessary to attribute land to a given child only once.
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FOREST CONSERVATION BENEFITS ;; ; this process provides farmers with energy (foods or money to buy food) based on the amount of forest which is protected. It implements payment for ecosystem services.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to attribute-conservation-benefits
 set conservation-benefits ((energy-from-1%-forest * minimum-forest-area) * ((100 - forest-cover * 100) / (100 - minimum-forest-area))) ; the benefits for each percent of forested area are multiplied by the total protected forested area. The benefits also depend on the remaining forest cover, thus increasing benefits with land scarcity (i.e. people do not receive benefits when the forest cover is still very high and establishing conservation areas is thus not required)
 let individual-benefits (conservation-benefits / (count turtles * 2 + sum [children-number] of turtles)) ; the total benefits are divided by the population (each agent is an household with 2 adults and a variable number of children)
 ask turtles [
    set energy-from-conservation (individual-benefits * (2 + children-number)) ; households obtain a different percentage of the total benefits proportional to household size
    set energy-target energy-needs * (children-number + 2) ; households adjust their energy targets to account for the conservation benefits, thus reducing the amount they aim to produce from agriculture and obtain from foraging
  ]
ask turtles [ ; this process lets farmers adjust their production target based on expected harvest losses, which are based on average disruptions AND previous year's ability of their plots to protect against climate disruptions
    let my-plots count patches with [owner = myself]
    ifelse my-plots > 0 and cultivated-plots > 0 [
      let expected-loss 0.6 * (1 - mean [climate-regulation] of patches with [owner = myself])
      set production-target (((energy-target * (1 + expected-loss)) * target-surplus) - energy-from-conservation)
    ][
      set production-target energy-target
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LAND USE PROCESS FOR PLOTS OTHER THAN S&B ;; ; this process determines how many and which forest plots are conversted to agricultural plots by lowland rice farmers and diverse farmers (i.e. all farmers not practicing shifting agriculture)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to plan-permanent-land-use
  let other-farmers (turtle-set paddy-producers diverse-producers)
  ask other-farmers [
    ask patches with [ owner = myself ] [ cultivate-diverse ] ; first cultivate already owned plots, this will update yields according to this year's disruption levels
    set energy-from-plots sum [expected-yield] of patches with [ owner = myself ] ; summing the expected yield of already owned plots to determine if more plots are needed
    if energy-from-plots < production-target [
      let enough-plots? false ; a true/talse condition that is updated when the sum of expected yield from owned plots covers the household energy needs: farmers keep clearing new plots until they have enough to feed their family
      while [not enough-plots?] [
        set forest-cover ( count patches with [land-use = "forest"] / count patches ) ; this updates the global variable 'forest cover'
        if forest-cover * 100 <= minimum-forest-area or forest-cover * 100 <= forest-desired [set enough-plots? true] ; the land clearing process stops when no available forest remains
        choose-new-plots ; choosing new plots, one at a time
        set energy-from-plots sum [expected-yield] of patches with [ land-use != "fallow" and owner = myself ]
        if energy-from-plots >= production-target [set enough-plots? true]
      ]
    ]
  ]
end

to choose-new-plots ; this process is called by the 'plan-permanent-land-use' above. Farmers convert forested plots into agricultural plots
  let target-patch min-one-of (patches with [land-use = "forest"]) [distance myself] ; farmers choose the closest forest patch
  if breed = diverse-producers and ( forest-cover * 100 >= minimum-forest-area and forest-cover * 100 >= forest-desired and forest-cover > 0) [ ; diverse producers convert forests into agroforests and home gardens
      ask target-patch [
       set land-use one-of ["agroforest" "garden"]
      cultivate-diverse]]
  if breed = paddy-producers and ( forest-cover * 100 >= minimum-forest-area and forest-cover * 100 >= forest-desired and forest-cover > 0 )  [ ; lowland rice producers convert forests into irrigated rice paddies
      ask target-patch [
      set land-use "paddy"
      cultivate-diverse]]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CULTIVATION PROCESS FOR PLOTS OTHER THAN S&B ;; ; this process follows the land use process, it updates variables for land plots which have been converted from forests into rice paddies, agroforests and home-gardens. plots in shifting agriculture have a distinct process later.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to cultivate-diverse
  set owner myself
  set years-cultivated-total years-cultivated-total + 1
  set years-fallowed 0 ; there should be no fallow, this is used for debugging
;; biodiversity values
  set soil-biodiversity (ifelse-value ;
    land-use = "paddy" [0.35]
    land-use = "agroforest" [soil-bd-agroforestry]
    land-use = "garden" [soil-bd-agroforestry])
  set perennial-cover (ifelse-value
    land-use = "paddy" [0]
    land-use = "agroforest" [perennial-cover-agroforestry]
    land-use = "garden" [perennial-cover-agroforestry])
  set production-diversity (ifelse-value
    land-use = "paddy" [0.1]
    land-use = "agroforest" [crop-diversity-agroforestry]
    land-use = "garden" [crop-diversity-agroforestry])
  set pcolor (ifelse-value
    land-use = "paddy" [93]
    land-use = "agroforest" [76]
    land-use = "garden" [74])
;; ecosystem services values
  set soil-fertility soil-biodiversity ; in the current implementation soil fertility is only computed based on soil biodiversity levels
  set pollination-service (1 - forest-pollination) + (forest-biodiversity * forest-pollination) ; pollination is computed at plot level based on forest biodiversity levels
  set expected-yield (ifelse-value ; the expected yield is computed based on the potential yield and soil fertility, as well as pollination and a land equivalent ratio for agroforests and home gardens
    land-use = "paddy" [max-paddy-yield * soil-fertility] ; pollinators do not impact rice yields
    land-use = "agroforest" [max-agroforest-yield * ((soil-fertility * 0.60) + (pollination-service * 0.40)) * land-equivalent-ratio] ; the land equivalent ratio is a yield multiplier for polycultures, it determines how much more (or less) total yields are compared to monocultures
    land-use = "garden" [max-homegarden-yield * ((soil-fertility * 0.60) + (pollination-service * 0.40)) * land-equivalent-ratio])
  set pest-regulation (disruption-shield * ((forest-biodiversity * 0.5) + (soil-biodiversity * 0.18) + (production-diversity * 0.16) + (perennial-cover * 0.16))) ; the percentage of pest pressure that the patch is shielded from
  set climate-regulation pest-regulation ; in the current implementation the equation is the same as for pest-regulation, so the value from the previous computation is used
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LAND USE PROCESSES FOR S&B ;; ; this process determines how many and which forest plots are conversted to agricultural plots by farmers practicing shifting agriculture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to plan-shifting-land-use
;; farmers first cultivate plots that they already own and which have not been cultivated more than their desired number of consecutive years
  ask tavy-producers [
   set energy-from-plots 0 ; The energy people obtained last year is deleted. the process below, as well as the next one, will sum the energy which can be expected from each plots chosen for cultivation this year
   let plots-to-cultivate-again? count patches with [ owner = myself and land-use = "S&B" and years-cultivated < desired-years-cultivated ]
   if plots-to-cultivate-again? > 0 [
   ask patches with [ owner = myself and land-use = "S&B" and years-cultivated < desired-years-cultivated ][ cultivate ]
   set energy-from-plots sum [expected-yield] of patches with [ owner = myself and land-use = "S&B" and years-cultivated < desired-years-cultivated ] ; farmers compute the expected yield from their plots that they cultivate again this year
    ]
  ]
;; if needed, farmers then clear aditional plots until their energy targets are achieved.
  ask tavy-producers [
    if (energy-from-plots) < production-target [
      let my-plots count patches with [ land-use != "degraded" and owner = myself ]
;; farmers aim to mitigate land degradation by reducing the amount of food they will produce this year. This allows longer fallows and thus soil fertility recovery.
      if my-plots > 0 [
       let yield-drop (1 - (mean [expected-yield] of patches with [ land-use != "degraded" and owner = myself ] / max-S&B-yield)) ; the percentage of yield lost due to soil fertility decrease
       if ( forest-cover * 100 <= minimum-forest-area or forest-cover * 100 <= forest-desired or forest-cover <= 0 ) [
          set production-target production-target - (production-target * (yield-drop * yield-drop-adaptation))
        ]
      ]
;; the code below implements the plot selection process: first, all plots owned but not cultivated in the process above are converted to fallows, then some are selected for cultivation in the 'choose-plot' process. The process stops when energy needs are covered
      ask patches with [owner = myself and land-use != "degraded" and years-cultivated >= desired-years-cultivated][
      set land-use "fallow"
      set years-cultivated 0
      ]
      let enough-plots? false ; a true/talse condition that is updated when the sum of expected harvests from converted plots covers the household energy needs
      while [not enough-plots?] [
        set forest-cover ( count patches with [land-use = "forest"] / count patches ) ; the forest cover is updated each time as farmers will stop clearing forest when no free forest patch remains
        choose-plots ; the procedure to choose a specific plot, described below
        set energy-from-plots sum [expected-yield] of patches with [ owner = myself and land-use = "S&B" ] ; the expected yield is updated each time an additional patch is cultivated
        if (energy-from-plots + energy-from-conservation) >= production-target [ set enough-plots? true ] ; the process stops when energy needs are covered
        let fallowed-plots? count patches with [ owner = myself and land-use = "fallow" ]
        if ticks > 1 and fallowed-plots? <= 0 [ set enough-plots? true ] ; the choosing process stops even if needs are not reached when only cultivated plots remain available
      ]
  ]
]
end

;; 'choose-plots' is called by the 'plan-shifting-land-use' process. It determines which plot farmer will cultivate first. Fallows with the desired vegetation cover are prioritised, then forest, then fallows with non-optimal vegetation cover (i.e. younger or degraded fallows).
to choose-plots
  let my-desired-fallow-state desired-fallow-state - 0.07 + (random 15 / 100) ; this creates small random noise in desired fallow state, avoiding syncrony accross the landscape
  let optimal-fallows count patches with [ owner = myself and land-use = "fallow" and perennial-cover >= my-desired-fallow-state ] ; farmers check whether they own fallows with a vegetation cover that they consider optimal (i.e. indicating good soil fertility)
  ifelse optimal-fallows > 0  [ ; if there is a fallowed patch that has an optimal vegetation cover, convert it to a cultivated patch, otherwise choose another patch
    let my-fallowed-plots patches with [ owner = myself and land-use = "fallow" and perennial-cover >= my-desired-fallow-state  ]
     ask one-of my-fallowed-plots with-max [perennial-cover][ ; farmers select one of their plots with optimal vegetation cover and cultivate it
    set land-use "S&B"
      cultivate
        ]
    ][
      choose-other-plot
    ]
end

;; 'choose-other-plots' below is called by 'choose-plots'. When farmers do not have fallows with the desired vegetation cover, they either clear a forest patch, if some remain, or cultivate their oldest fallow
to choose-other-plot
  ifelse (forest-cover * 100 >= minimum-forest-area and forest-cover * 100 >= forest-desired and forest-cover > 0) [ ; if free forest patches remain, farmers choose one for cultivation
    let target-patch min-one-of (patches with [land-use = "forest"]) [distance myself] ; choose the closest forest patch to clear
    ask target-patch [
      set land-use "S&B"
      cultivate
      ]
  ][
    let fallowed-plots? count patches with [ owner = myself and land-use = "fallow" ] ; if no free forest patch remain, farmers choose their best fallow instead. (best = highest vegetation cover, indicating better soil fertility)
    if fallowed-plots? > 0 [
      let my-fallowed-plots patches with [ owner = myself and land-use = "fallow" ]
       ask one-of my-fallowed-plots with-max [perennial-cover][
        set land-use "S&B"
        cultivate
        ]
      ]
   ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CULTIVATION PROCESS FOR S&B ;; ; this process follows the land use process, it updates variables for land plots which have been converted from forests into shifting agriculture. It is different from the process for other agricultural practices because soil biodiversity and vegetation cover are dynamic and must thus be computed for each time step.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to cultivate
  set owner myself
  set degradation degradation + (degradation-pace / improved-fallow-rate) ; each cultivation causes 10% land degradation according to Wood et al. 2017
  set years-cultivated years-cultivated + 1
  set years-cultivated-total years-cultivated-total + 1
  set years-fallowed 0
;; biodiversity below
  set perennial-cover tree-cover-after-clearing ; cultivation removes all tree cover
  set production-diversity production-diversity-S&B ; only upland rice is cultivated
;; ecosystem services below
;  set pollination-service (perennial-cover + forest-biodiversity) / 2 ; irrelevant for rice
  set soil-fertility soil-biodiversity
  set expected-yield max-S&B-yield * soil-fertility ; for upland rice, the expected yield is only determined by the potential yield and soil fertility
  set pest-regulation (disruption-shield * ((forest-biodiversity * 0.5) + (soil-biodiversity * 0.18) + (production-diversity * 0.16) + (perennial-cover * 0.16)))  ; the percentage of pest pressure that the patch is shielded from
  set climate-regulation pest-regulation ; the percentage of climate pressure that the patch is shielded from, equal to pest-regulation
  set pcolor scale-color brown expected-yield -40 (max-S&B-yield * 1.2) ; the cultivated patch will be a light brown if expected yield is at a maximum, and become a darker brown with diminishing yield, showing soil fertility loss
  set soil-biodiversity soil-biodiversity * soil-BD-loss-pace  ; soil biodiversity is reduced by 40% each time the plot is cultivated. Each successive cultivated year thus has a lower impact.
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HARVEST FOR ALL PLOTS ;; ; this process computes the achieved yield, i.e. the yield after losses from climatic and pest pressure. This process applies to all agricultural plots
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to harvest
;; yield losses to climatic and pest pressure are computed
  set vegetation-cover mean [perennial-cover] of patches ; updates the global variable 'vegetation-cover' after changes made to agricultural plots
  ask patches with [land-use != "forest" and land-use != "degraded"] [
    let plot-abiotic-impact (abiotic-disruption * (1 - climate-regulation)) ; each agricultural plot computes its level of climatic pressure, based on the global climatic disruption and the plot-level climate-regulation.
    let plot-biotic-impact (biotic-disruption * (1 - pest-regulation)) ; each agricultural plot computes its level of pest pressure, based on the global pest disruption and the plot-level pest-regulation.
    set achieved-yield expected-yield - ( expected-yield * (plot-abiotic-impact + plot-biotic-impact) / 100 ) ; each agricultural plot computes yield loss from climatic and pest pressure
  ]
;; farmers obtain energy from all their plots, based on their respective yields
  ask turtles [
    set production sum [achieved-yield] of patches with [ land-use != "fallow" and land-use != "degraded" and owner = myself ] ; the total agricultural production per household is calculated
    set energy-from-plots production ; it is necessary to have two variables as the energy from plot will change with food stock management below
;; after harvest, food stocks are managed: if surpluses have been harvested, they get stored, if harvest are insufficient, stocks are used
    if food-stock > 0 [set food-stock food-stock * ( 1 - food-stock-loss) ] ; assuming that a third of the surplus is either lost of used for non-food activities
    ifelse production > (energy-needs * (children-number + 2)) [ ; if the harvest is greater than family needs, farmers add surplus to stock, and eat according to needs
      set food-stock (food-stock + (production - (energy-needs * (children-number + 2))))
      set energy-from-plots (energy-needs * (children-number + 2))
     ][ ; if the harvest does not cover the need, farmers obtain energy from the stock
      let energy-deficit (energy-needs * (children-number + 2)) - energy-from-plots ; computes how much is needed from the stock
      ifelse food-stock >= energy-deficit [ ; if the stock covers the needs
      set food-stock (food-stock - energy-deficit) ; withdraw what is needed from the stock
      set energy-from-plots (energy-needs * (children-number + 2))
      ][ ; if the stock does not cover the needs
      set energy-from-plots ( energy-from-plots + food-stock) ; withdraw everything from the stock
      set food-stock 0 ; empty stock
      ]
    ]
  ]
;; diet diversity is calculated based on land-use, it will be updated later, in the foraging process
  let rice-farmers (turtle-set paddy-producers tavy-producers) ; parameterised based on latest regional statistics (RGPH-3, values for Alaotra-mangoro) and assuming that diet diversity decrease when food insecurity increases
  ask rice-farmers [
    let energy-gap energy-from-plots / (energy-needs * (children-number + 2))
    ifelse energy-gap >= 1 [
      set food-groups 3 ][
      set food-groups round ( 3 * energy-gap )
    ]
 ]
  ask diverse-producers [ ; parameterised based on the crop portfolio
    let energy-gap energy-from-plots / (energy-needs * (children-number + 2))
    ifelse energy-gap >= 1 [
      set food-groups 6 ][
      set food-groups round ( 6 * energy-gap )
    ]
 ]
end



;;;;;;;;;;;;;;
;; FORAGING ;; ; this process implements the foraging (collection of wild foods). Farmers forage opportunistically each year and purposefully when agriculture does not cover their energy needs.
;;;;;;;;;;;;;;

to forage
;; farmers forage only if some wild foods remain available and at a level above foraging quotas (if implemented)
  let yearly-edible-species-diversity edible-species-diversity; the diversity at the beginning of the year. This has been added to prevent hierachy between agents: as each agent reduces diversity, which originally caused agents foraging after other to obtain less
  ifelse edible-species-diversity > species-loss-per-foraging and edible-species-diversity >= foraging-quota [ ; this code allows to skip the foraging process for all farmers
    ask turtles [
      set energy-from-forest 0 ; starting at 0 for this turn
      if edible-species-diversity > species-loss-per-foraging ; this is specific to each farmer and stops individual farmers from foraging if values ar too low. it is necessary as there are multiple farmers each reducing wild food availability.
      [
;; the additional amount of energy farmers obtain from foraging
       set opportunistic-foraging (opportunistic-foraging + yearly-edible-species-diversity * foraging-intensity)
       set energy-from-forest (1200 * yearly-edible-species-diversity * foraging-intensity)
       set edible-species-diversity (edible-species-diversity - (species-loss-per-foraging * edible-species-diversity * foraging-intensity))
;; the additional diet diversity farmers obtain from foraging: rice farmers can benefit more than diverse farmers as diverse farmers already produce most food groups that could be obtained from foraging
      ifelse food-groups <= 3 [
          set food-groups food-groups + (yearly-edible-species-diversity * 5) ; rice farmers can get up to 4 additional food groups from foraging
        ][
          set food-groups food-groups + (yearly-edible-species-diversity * 2) ; diverse farmers have a higher baseline and can only get up to 2 additionnal food groups from foraging
        ]
      set food-groups round food-groups ; the value is rounded to an integer
      ]
;; after having foraged opportunistically, farmers will forage purposefully if their energy needs are not covered.
;; purposeful foraging is implemented by having farmers forage up to 8 times. However these foraging events have a lower value in both the energy obtained and foraging impact, allowing a more granular response to food insecurity.
    repeat 8 [
      if (energy-from-plots + energy-from-forest + energy-from-conservation) < energy-target and (edible-species-diversity > species-loss-per-foraging / 3) [
        set purposeful-foraging (purposeful-foraging + (edible-species-diversity))
        set energy-from-forest energy-from-forest + ((1200 / 3) * edible-species-diversity)
        set edible-species-diversity (edible-species-diversity - ((species-loss-per-foraging / 3) * edible-species-diversity)) ; 2.5% per foraging event
        set energy-target energy-target * 0.98 ; the energy target is reduced after each foraging event, to ensure that foraging intensity is proportional to food insecurity depth
      ]
    ]
   ]
  ]
  [
    set opportunistic-foraging 0 ; this is the second option for the 'ifelse' at the beginning of the foraging process: if no wild foods remain people do not forage
    ask turtles [set energy-from-forest 0] ; this ensures that the value from the last turn is updated when no foraging occurs
  ]
;; after foraging, wild edible species recover for next year
  if edible-species-diversity > 0 [set edible-species-diversity edible-species-diversity * species-recovery-pace ] ; wild species recover non linearly: the greater its value, the greater the recovery (up to a ceiling set below)
  if edible-species-diversity > forest-biodiversity [set edible-species-diversity forest-biodiversity]; general forest-biodiversity creates a ceiling for maximum wild species diversity
end

;;;;;;;;;;;;;;;;;;
;; MOVE TO PLOT ;; ; this process moves farmers to one of their cultivated plots. It is used for visualisation only.
;;;;;;;;;;;;;;;;;;

to move-to-plot
  ask turtles [
    let my-cultivated-patches count patches with [ land-use != "fallow" and land-use != "degraded" and owner = myself ]
    if my-cultivated-patches > 0 [ move-to one-of patches with [ land-use != "fallow" and land-use != "degraded"  and owner = myself ]]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;
;; FALLOW PROCESSES ;; ; this process allows fallows to recover soil biodiversity and vegetation cover incrementally each year. It also controls soil degradation
;;;;;;;;;;;;;;;;;;;;;;;;

to recover-fallows
;; when plots are too degraded they lose their ability to recover soil biodiversity and vegetation, and thus remain unproductive as well as unable to host biodiversity.
  let non-degraded-patches patches with [land-use != "degraded"]
  ask non-degraded-patches with [ degradation >= 0.7 ][ ; after 7 cultivation cycles plots are too degraded to be cultivated again. Value based on literature.
  set land-use "degraded"
  set soil-biodiversity 0
  set perennial-cover 0
  set production-diversity 0
  set climate-regulation 0
  set pest-regulation 0
  set soil-fertility 0
  set expected-yield 0
  set achieved-yield 0
  set years-cultivated 0
  set years-fallowed 0
  set pcolor 13 ; degraded plots are set a dark red color to be easily visible
  ]
;; fallows which are not degraded recover soil biodiversity and vegetation cover
  ask patches with [ land-use = "fallow" ] [
  set years-cultivated 0
  set years-fallowed years-fallowed + 1
  if years-fallowed > (soil-BD-recovery-delay / improved-fallow-rate) [ set soil-biodiversity soil-biodiversity + (1 - degradation) * (soil-BD-recovery-pace * ((1 - soil-biodiversity) + 0.4)) * improved-fallow-rate ] ; soil biodiversity start recovering after 3 years.
  set soil-fertility soil-biodiversity
  if soil-biodiversity > 1 [ set soil-biodiversity 1 ] ; this ensures that values never exceed 1
  set perennial-cover perennial-cover + (1 - degradation) * (tree-regrowth-pace * (1 - perennial-cover)) * improved-fallow-rate  ; vegetation cover recovers from the first year
  if perennial-cover > 1 [ set perennial-cover 1 ] ; this ensures that values never exceed 1
  set expected-yield (soil-biodiversity * max-S&B-yield) ; the expected yield is updated
  set achieved-yield 0
  set pcolor scale-color lime years-fallowed 80 -4 ; the cells become a darker green with years fallowed
  ]
end

;;;;;;;;;;;;;;;;;;;;;;
;; FARMER PROCESSES ;; ; finally, farmers variables are updated to determine nutrition outcomes
;;;;;;;;;;;;;;;;;;;;;;

to update-farmer-variables
ask turtles [
  set energy-per-capita (energy-from-plots + energy-from-forest + energy-from-conservation) / (children-number + 2 )
  let my-current-plots count patches with [ owner = myself and land-use != "fallow" and land-use != "degraded"] ; keeps track of how many plots are cultivated
  set cultivated-plots my-current-plots ; variable used to update display plot
  let my-current-fallows count patches with [ owner = myself and land-use = "fallow" ]
  set fallowed-plots my-current-fallows
    let nutritional-status (energy-per-capita / energy-needs)
    ifelse nutritional-status >= 0.95 [set color green] [set color orange]
    if nutritional-status < 0.5 [set color red]
  ]
end

;; if this process is activated in the user interface, farmers will clear one protected forest patch if they experience dramatic food insecurity
to clear-protected-patch
ask tavy-producers [
    let forest-remaining ( count patches with [land-use = "forest"] / count patches )
    if (energy-per-capita / energy-needs) < 0.33 and forest-remaining > 0 [ ; if farmers have less than a third of their nutritional goals covered, and there is some forest remaining, they choose ONE forest patch and clear it
    let target-patch min-one-of (patches with [land-use = "forest"]) [distance myself] ; choose the closest forest patch to clear
    ask target-patch [
      set land-use "S&B"
      cultivate
      ]
    ]
  ]
ask paddy-producers [
    let forest-remaining ( count patches with [land-use = "forest"] / count patches )
    if (energy-per-capita / energy-needs) < 0.33 and forest-remaining > 0 [ ; if farmers have less than a third of their nutritional goals covered, and there is some forest remaining, they choose ONE forest patch and clear it
    let target-patch min-one-of (patches with [land-use = "forest"]) [distance myself] ; choose the closest forest patch to clear
    ask target-patch [
      set land-use "paddy"
      cultivate
      ]
    ]
  ]
ask diverse-producers [
    let forest-remaining ( count patches with [land-use = "forest"] / count patches )
    if (energy-per-capita / energy-needs) < 0.33 and forest-remaining > 0 [ ; if farmers have less than a third of their nutritional goals covered, and there is some forest remaining, they choose ONE forest patch and clear it
    let target-patch min-one-of (patches with [land-use = "forest"]) [distance myself] ; choose the closest forest patch to clear
    ask target-patch [
      set land-use "agroforest"
      cultivate
      ]
    ]
  ]
update-farmer-variables ; farmer nutritional status is updated
end
@#$#@#$#@
GRAPHICS-WINDOW
4
10
446
696
-1
-1
7.61404
1
10
1
1
1
0
1
1
1
-28
28
-44
44
1
1
1
ticks
30.0

BUTTON
630
538
696
571
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
631
578
694
611
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
631
617
694
650
step
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1676
589
1873
622
desired-fallow-state
desired-fallow-state
0
1
0.5
0.1
1
NIL
HORIZONTAL

PLOT
590
134
750
254
plots per HH
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"pen-6" 1.0 0 -15575016 true "" "plot mean [fallowed-plots] of turtles"
"pen-7" 1.0 0 -10263788 true "" "plot mean [cultivated-plots] of turtles"

PLOT
1079
134
1239
254
average yield
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -9276814 true "" "if ticks > 0 [ plot mean [expected-yield] of patches with [land-use = \"S&B\"]]"
"pen-1" 1.0 0 -955883 true "" ";if ticks > 0 [ plot mean [achieved-yield] of patches with [land-use != \"fallow\" and land-use != \"degraded\" and owner != nobody]]"
"pen-2" 1.0 0 -14454117 true "" "if ticks > 0 [ plot mean [achieved-yield] of patches with [land-use = \"paddy\"]]"
"pen-3" 1.0 0 -15302303 true "" "if ticks > 0 [ plot mean [achieved-yield] of patches with [land-use = \"agroforest\" or land-use = \"garden\"]]"
"pen-4" 1.0 0 -8431303 true "" "if ticks > 0 [ plot mean [achieved-yield] of patches with [land-use = \"S&B\"]]"

TEXTBOX
6
703
253
721
Landscape area: 507 ha = 5.07km2
11
0.0
1

PLOT
590
12
750
132
pop density / km2
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks > 0 [ plot (count turtles * 2 + ( sum [children-number] of turtles )) / 5.073 ] "

PLOT
1242
379
1402
499
% people in energy deficit of 12 25 50 75 %
NIL
NIL
0.0
1.0
0.0
12.0
true
false
"" ""
PENS
"pen-4" 1.0 0 -4539718 true "" "plot 25"
"pen-5" 1.0 0 -4539718 true "" "plot 50"
"pen-6" 1.0 0 -4539718 true "" "plot 75"
"pen-7" 1.0 0 -4539718 true "" "plot 100"
"ok" 1.0 0 -13840069 true "" "if ticks > 1 [ plot count turtles with [energy-per-capita <= (energy-needs * 0.88)]  / (count turtles) * 100 ]; 88%"
"meh" 1.0 0 -817084 true "" "if ticks > 1 [ plot count turtles with [energy-per-capita <= (energy-needs * 0.75)]  / (count turtles) * 100 ]; 75% "
"bad" 1.0 0 -2674135 true "" "if ticks > 1 [ plot count turtles with [energy-per-capita <= (energy-needs * 0.5)] / (count turtles) * 100 ]; 50%"
"critical" 1.0 0 -10873583 true "" "if ticks > 1 [ plot count turtles with [energy-per-capita <= (energy-needs * 0.25)] / (count turtles) * 100 ]; 25%"

PLOT
753
134
913
254
foraging pressure
NIL
NIL
0.0
0.0
0.0
1.0
true
false
"" ""
PENS
"total" 1.0 0 -1513240 true "" "if ticks > 0 [plot (opportunistic-foraging + (purposeful-foraging / 3)) / count turtles]"
"purposeful" 1.0 0 -8053223 true "" "if ticks > 0 [plot (purposeful-foraging / 3) / count turtles]"
"opportunistic" 1.0 0 -16777216 true "" "if ticks > 0 [plot opportunistic-foraging / count turtles]"

PLOT
590
256
750
376
1 / harvest lost
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"total disruption" 1.0 0 -1513240 true "" "if ticks > 0 [plot 100 - (abiotic-disruption + biotic-disruption)] "
"pen-2" 1.0 0 -2674135 true "" ";if ticks > 0 [plot  (mean [achieved-yield] of patches with [land-use != \"fallow\" and land-use != \"degraded\" and owner != nobody]) / (mean [expected-yield] of patches with [land-use != \"fallow\" and land-use != \"degraded\" and owner != nobody]) * 100]"
"pen-4" 1.0 0 -7500403 true "" "plot 50"
"S&B" 1.0 0 -6459832 true "" "if ticks > 0 [plot  (mean [achieved-yield] of patches with [land-use = \"S&B\"]) / (mean [expected-yield] of patches with [land-use = \"S&B\"]) * 100]"
"Agroeco" 1.0 0 -14439633 true "" "if ticks > 0 [plot  (mean [achieved-yield] of patches with [land-use = \"agroforest\"]) / (mean [expected-yield] of patches with [land-use = \"agroforest\"]) * 100]"
"paddy" 1.0 0 -14454117 true "" "if ticks > 0 [plot  (mean [achieved-yield] of patches with [land-use = \"paddy\"]) / (mean [expected-yield] of patches with [land-use = \"paddy\"]) * 100]"

PLOT
1079
12
1239
132
Soil biodiversity
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"pen-2" 1.0 0 -4539718 true "" "plot 50"
"pen-5" 1.0 0 -5298144 true "" "plot mean [soil-biodiversity] of patches with [land-use = \"S&B\" ] * 100"
"pen-6" 1.0 0 -14454117 true "" "plot mean [soil-biodiversity] of patches with [land-use = \"paddy\" ] * 100"
"pen-7" 1.0 0 -13840069 true "" "plot mean [soil-biodiversity] of patches with [land-use = \"agroforest\" or land-use = \"garden\"] * 100"
"pen-8" 1.0 0 -14333415 true "" "plot mean [soil-biodiversity] of patches with [land-use = \"fallow\" ] * 100"

PLOT
916
12
1076
132
plots and fallows (ha)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -7171555 true "" "if ticks > 0 [ plot count patches with [land-use != \"fallow\" and owner != nobody] / 10] "
"pen-1" 1.0 0 -15040220 true "" "if ticks > 0 [ plot count patches with [land-use = \"fallow\" ]/ 10] "

SLIDER
992
510
1144
543
minimum-forest-area
minimum-forest-area
0
100
0.0
2.5
1
NIL
HORIZONTAL

SLIDER
992
545
1145
578
foraging-quota
foraging-quota
0
1
0.0
.05
1
NIL
HORIZONTAL

PLOT
1242
134
1402
254
% wild species
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot edible-species-diversity * 100"

PLOT
1242
503
1402
623
Energy sources and needs
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-3" 1.0 0 -7500403 true "" "plot energy-needs"
"pen-4" 1.0 0 -11053225 true "" "plot energy-needs / 4"
"charcoal" 1.0 0 -955883 true "" ";plot mean [energy-from-charcoal] of turtles / (2 + mean [children-number] of turtles)"
"harvest" 1.0 0 -6459832 true "" "plot mean [energy-from-plots] of turtles / (2 + mean [children-number] of turtles)"
"forest" 1.0 0 -15575016 true "" "plot mean [energy-from-forest] of turtles / (2 + mean [children-number] of turtles)"
"total" 1.0 0 -16777216 true "" "plot mean [energy-per-capita] of turtles"
"conservation" 1.0 0 -1184463 true "" "plot mean [energy-from-conservation] of turtles / (2 + mean [children-number] of turtles)"

SLIDER
750
615
910
648
S&B-households
S&B-households
0
60
4.0
1
1
NIL
HORIZONTAL

SLIDER
750
545
910
578
paddy-households
paddy-households
0
80
0.0
1
1
NIL
HORIZONTAL

SLIDER
750
580
910
613
diverse-households
diverse-households
0
400
0.0
1
1
NIL
HORIZONTAL

PLOT
916
134
1076
254
Pollin & pest regulation
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -15582384 true "" "if ticks > 0 [plot mean [climate-regulation] of patches with [owner != nobody] * 100]"
"pen-1" 1.0 0 -15575016 true "" "if ticks > 0 [plot mean [pest-regulation] of patches with [owner != nobody] * 100]"
"pen-2" 1.0 0 -4079321 true "" "if ticks > 0 [plot mean [pollination-service] of patches with [land-use = \"garden\"] * 100]"

SLIDER
992
580
1145
613
improved-fallow-rate
improved-fallow-rate
0.4
10
1.0
0.2
1
NIL
HORIZONTAL

PLOT
1242
257
1402
377
diet diversity
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -10263788 true "" "plot mean [food-groups] of tavy-producers"
"pen-1" 1.0 0 -14985354 true "" "plot mean [food-groups] of paddy-producers"
"pen-2" 1.0 0 -15575016 true "" "plot mean [food-groups] of diverse-producers"
"pen-3" 1.0 0 -5298144 true "plot 2" "plot 5"

MONITOR
455
12
584
57
population
count turtles * 2 \n+ sum [children-number] of turtles
17
1
11

MONITOR
455
61
585
106
ha owned / HH
count patches with [owner != nobody] / count turtles / 10
3
1
11

SLIDER
1676
694
1874
727
yield-drop-adaptation
yield-drop-adaptation
0.1
10
0.4
0.1
1
NIL
HORIZONTAL

PLOT
1242
12
1402
132
forest cover & BD
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot forest-biodiversity * 100"
"pen-1" 1.0 0 -3026479 true "" "plot 60"
"pen-2" 1.0 0 -14439633 true "" "plot forest-cover * 100"
"pen-3" 1.0 0 -3026479 true "" "plot 30"
"pen-4" 1.0 0 -3026479 true "" "plot 20"
"pen-5" 1.0 0 -2674135 true "" "plot fc_threshold * 100"
"pen-6" 1.0 0 -5509967 true "" "plot vegetation-cover * 100"
"pen-7" 1.0 0 -7500403 true "" "plot 7.5"

PLOT
753
12
913
132
max / mean fallow age
NIL
NIL
0.0
5.0
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if ticks > 1 [plot mean [years-fallowed] of patches with [land-use = \"fallow\"]]"
"pen-1" 1.0 0 -8053223 true "" "if ticks > 1 [plot max [years-fallowed] of patches with [land-use = \"fallow\"]]"

PLOT
1079
257
1239
377
% degraded plots
NIL
NIL
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count patches with [land-use = \"degraded\"] / 5070 * 100"

MONITOR
455
202
585
247
max years cultivated
max [years-cultivated] of patches with [land-use = \"S&B\"]
1
1
11

MONITOR
455
108
585
153
ha fallowed / HH
mean [fallowed-plots] of turtles / 10
2
1
11

MONITOR
455
155
585
200
ha degraded / HH
count patches with [land-use = \"degraded\"] / count turtles / 10
2
1
11

PLOT
916
257
1076
377
degradation level
NIL
NIL
0.0
10.0
0.0
0.7
true
false
"" ""
PENS
"default" 1.0 0 -10873583 true "" "plot mean [degradation] of patches with [land-use != \"forest\"]"
"pen-2" 1.0 0 -4079321 true "" "plot mean [degradation] of patches with [land-use = \"S&B\"]"
"pen-3" 1.0 0 -15040220 true "" "plot mean [degradation] of patches with [land-use = \"fallow\"]"

INPUTBOX
750
650
910
710
max-pop-density
5000.0
1
0
Number

SLIDER
1676
624
1873
657
desired-years-cultivated
desired-years-cultivated
1
5
1.0
1
1
NIL
HORIZONTAL

PLOT
753
256
913
376
Food stock per hab
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -8431303 true "" "plot mean [food-stock] of tavy-producers / 4"
"pen-1" 1.0 0 -12087248 true "" "plot mean [food-stock] of diverse-producers / 4"
"pen-2" 1.0 0 -13791810 true "" "plot mean [food-stock] of paddy-producers / 4"
"pen-3" 1.0 0 -12895429 true "" "plot 876"

INPUTBOX
992
615
1145
675
energy-from-1%-forest
0.0
1
0
Number

PLOT
1079
379
1239
499
fallows tree cover
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot 100 * mean [perennial-cover] of patches with [land-use = \"fallow\"]"
"pen-1" 1.0 0 -15637942 true "" "plot 100 * mean-tree-cover "

INPUTBOX
1676
729
1875
789
target-surplus
1.1
1
0
Number

CHOOSER
993
678
1146
723
clear-patch-if-starving
clear-patch-if-starving
"yes" "no"
1

SLIDER
1465
47
1664
80
disruption-level
disruption-level
0
100
60.0
1
1
NIL
HORIZONTAL

SLIDER
1467
396
1666
429
degradation-pace
degradation-pace
0
0.225
0.1
0.025
1
NIL
HORIZONTAL

SLIDER
1467
257
1666
290
potential-yield-weight
potential-yield-weight
0.6
1.5
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
1467
292
1666
325
production-diversity-S&B
production-diversity-S&B
0.1
0.5
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
1467
465
1666
498
soil-BD-recovery-delay
soil-BD-recovery-delay
0
5
3.0
1
1
NIL
HORIZONTAL

SLIDER
1468
500
1666
533
soil-BD-recovery-pace
soil-BD-recovery-pace
0.029
0.155
0.065
0.012
1
NIL
HORIZONTAL

SLIDER
1467
431
1666
464
soil-BD-loss-pace
soil-BD-loss-pace
0.1
1
0.6
0.1
1
NIL
HORIZONTAL

SLIDER
1467
327
1665
360
tree-cover-after-clearing
tree-cover-after-clearing
0
0.36
0.0
0.04
1
NIL
HORIZONTAL

SLIDER
1467
361
1665
394
tree-regrowth-pace
tree-regrowth-pace
0.035
0.155
0.065
0.012
1
NIL
HORIZONTAL

SLIDER
1676
659
1873
692
desired-forest-cover
desired-forest-cover
0
32
7.5
0.5
1
NIL
HORIZONTAL

SLIDER
1465
117
1665
150
forest-BD-loss-threshold
forest-BD-loss-threshold
0.1
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
1466
151
1665
184
species-loss-per-foraging
species-loss-per-foraging
0.025
0.125
0.075
0.025
1
NIL
HORIZONTAL

SLIDER
1467
222
1665
255
species-recovery-pace
species-recovery-pace
1
5
2.5
0.5
1
NIL
HORIZONTAL

SLIDER
1468
535
1665
568
food-stock-loss
food-stock-loss
0.15
0.75
0.3
0.15
1
NIL
HORIZONTAL

SLIDER
1466
186
1666
219
foraging-intensity
foraging-intensity
0
2
0.7
0.1
1
NIL
HORIZONTAL

PLOT
916
379
1076
499
total production
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -12440034 true "" "plot sum [production] of tavy-producers"
"pen-1" 1.0 0 -14439633 true "" "plot sum [energy-from-forest] of turtles"
"pen-2" 1.0 0 -7171555 true "" "plot conservation-benefits"
"pen-3" 1.0 0 -13791810 true "" "plot sum [production] of paddy-producers"
"pen-4" 1.0 0 -15302303 true "" "plot sum [production] of diverse-producers"

SLIDER
1465
82
1664
115
disruption-shield
disruption-shield
0
1
1.0
0.1
1
NIL
HORIZONTAL

TEXTBOX
1669
54
1919
75
intensity of biotic and abiotic disruptions (60)
11
0.0
1

TEXTBOX
1671
87
1869
115
% disruption ES will shield from (1)
11
0.0
1

TEXTBOX
1671
123
1948
151
% forest cover below which forest BD drops (0.3)
11
0.0
1

TEXTBOX
1672
406
1872
434
% degradation per cultivation (0.1)
11
0.0
1

SLIDER
750
511
910
544
simulation-duration
simulation-duration
60
200
120.0
5
1
NIL
HORIZONTAL

TEXTBOX
1673
159
1945
187
% wild species lost each time a HH forages (0.075)
11
0.0
1

TEXTBOX
1674
196
2000
224
Amount of energy farmers aim to obtain from foraging (0.7)
11
0.0
1

TEXTBOX
1672
232
1955
260
how fast wild species recover each year (2.5)\n
11
0.0
1

TEXTBOX
1672
337
1862
366
% perennials left in S&B fields (0)
11
0.0
1

TEXTBOX
1672
266
1840
285
potential yield multiplier (1)
11
0.0
1

TEXTBOX
1670
302
1820
320
crop diversity in S&B (0.1)
11
0.0
1

TEXTBOX
1671
372
1936
400
how fast vegetation recovers in fallows (0.065)
11
0.0
1

TEXTBOX
1674
442
1966
484
% of soil BD left after cultivation in S&B fields (0.6)
11
0.0
1

TEXTBOX
1673
473
1888
491
N. years before soil BD recovers (3)
11
0.0
1

TEXTBOX
1671
511
1933
540
how fast soil BD recovers in fallows (0.065)
11
0.0
1

TEXTBOX
1673
542
1920
570
% of food stock lost per year (0.3)
11
0.0
1

PLOT
590
379
750
499
production per HH
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [production] of tavy-producers"
"pen-1" 1.0 0 -14439633 true "" "plot mean [energy-from-forest] of turtles"
"pen-2" 1.0 0 -7171555 true "" "plot (conservation-benefits / count turtles)"
"pen-3" 1.0 0 -14454117 true "" "plot mean [production] of paddy-producers"
"pen-4" 1.0 0 -15302303 true "" "plot mean [production] of diverse-producers"
"pen-5" 1.0 0 -7500403 true "" "plot energy-needs * 4 ; energy needs of a HH"

TEXTBOX
1152
648
1302
666
2500 = 25%
11
0.0
1

PLOT
753
379
913
499
production targets
NIL
NIL
0.0
10.0
100.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -12440034 true "" "plot 100 * mean [production-target] of tavy-producers / mean [energy-target] of tavy-producers"
"pen-1" 1.0 0 -14985354 true "" "plot 100 * mean [production-target] of paddy-producers / mean [energy-target] of paddy-producers"
"pen-2" 1.0 0 -15637942 true "" "plot 100 * mean [production-target] of diverse-producers / mean [energy-target] of diverse-producers"

CHOOSER
1464
779
1665
824
stochastic-disruption
stochastic-disruption
"yes" "no"
0

SLIDER
1464
828
1665
861
deter-disruption-level
deter-disruption-level
0
100
30.0
1
1
NIL
HORIZONTAL

SLIDER
1466
589
1660
622
soil-bd-agroforestry
soil-bd-agroforestry
0
1
0.9
0.1
1
NIL
HORIZONTAL

SLIDER
1466
625
1661
658
perennial-cover-agroforestry
perennial-cover-agroforestry
0
1
0.7
0.1
1
NIL
HORIZONTAL

SLIDER
1466
663
1662
696
crop-diversity-agroforestry
crop-diversity-agroforestry
0
1
0.8
0.1
1
NIL
HORIZONTAL

SLIDER
1466
700
1663
733
LER
LER
0
2
1.3
0.1
1
NIL
HORIZONTAL

SLIDER
1466
736
1664
769
forest-pollination
forest-pollination
0
1
0.25
0.1
1
NIL
HORIZONTAL

TEXTBOX
1466
11
1732
42
The variables below are not meant to be modified. They are here for the sensitivity analysis
12
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person farmer
false
0
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -1 true false 60 195 90 210 114 154 120 195 180 195 187 157 210 210 240 195 195 90 165 90 150 105 150 150 135 90 105 90
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -13345367 true false 120 90 120 180 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 180 90 172 89 165 135 135 135 127 90
Polygon -6459832 true false 116 4 113 21 71 33 71 40 109 48 117 34 144 27 180 26 188 36 224 23 222 14 178 16 167 0
Line -16777216 false 225 90 270 90
Line -16777216 false 225 15 225 90
Line -16777216 false 270 15 270 90
Line -16777216 false 247 15 247 90
Rectangle -6459832 true false 240 90 255 300

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Scenario_2_120y_4SB_50hab_per_km_forest0-40_desired_0" repetitions="4000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt;= 120</exitCondition>
    <metric>forest-cover</metric>
    <metric>forest-biodiversity</metric>
    <metric>edible-species-diversity</metric>
    <metric>mean [achieved-yield] of patches with [land-use = "S&amp;B"] ; yield</metric>
    <metric>(1 - (mean [achieved-yield] of patches with [land-use = "S&amp;B"]) / (mean [expected-yield] of patches with [land-use = "S&amp;B"])) * 100; harvest loss</metric>
    <metric>mean [production] of turtles / 3504</metric>
    <metric>mean [energy-per-capita] of turtles / 876</metric>
    <metric>mean [food-groups] of turtles * 10 ; diet diversity</metric>
    <metric>count turtles * 4 / 5.073 ; pop density</metric>
    <metric>mean [food-stock] of turtles / 4 / 876 ; indiv needs covered by food stocks</metric>
    <metric>876 / mean [energy-from-forest] of turtles / 4 ; % needs covered by wild foods</metric>
    <metric>sum [production] of turtles</metric>
    <metric>count patches with [land-use = "fallow"] / count patches with [land-use != "forest"] ; % fallows</metric>
    <metric>count patches with [land-use = "fallow"] / count patches with [land-use = "S&amp;B"] ; ratio of fallowed to cultivated plots</metric>
    <metric>count patches with [land-use = "degraded"] / count patches with [land-use != "forest"] ; % ag plots degraded</metric>
    <metric>mean [degradation] of patches with [land-use != "forest"] * 10 ; degradation of agricultural patches</metric>
    <metric>mean [perennial-cover] of patches with [land-use = "fallow"] ; average fallow state</metric>
    <metric>mean-tree-cover ; vegetation cover of non-forest patches</metric>
    <metric>mean [soil-biodiversity] of patches with [land-use = "fallow"]</metric>
    <metric>mean [soil-fertility] of patches with [land-use = "S&amp;B"]</metric>
    <metric>mean [soil-fertility] of patches with [land-use = "S&amp;B" or land-use = "degraded"]</metric>
    <metric>mean [climate-regulation] of patches with [land-use = "S&amp;B"]</metric>
    <metric>mean [production-target] of turtles</metric>
    <metric>sum [cultivated-plots] of turtles</metric>
    <enumeratedValueSet variable="improved-fallow-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target-surplus">
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disruption-level">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="species-loss-per-foraging">
      <value value="0.075"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degradation-pace">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-yield-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-duration">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forest-BD-loss-threshold">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="production-diversity-S&amp;B">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-bd-agroforestry">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S&amp;B-households">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deter-disruption-level">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crop-diversity-agroforestry">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-fallow-state">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forest-pollination">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-recovery-pace">
      <value value="0.065"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="foraging-intensity">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paddy-households">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochastic-disruption">
      <value value="&quot;yes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="species-recovery-pace">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="foraging-quota">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-forest-cover">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-years-cultivated">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tree-regrowth-pace">
      <value value="0.065"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tree-cover-after-clearing">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-patch-if-starving">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-loss-pace">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-1%-forest">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-drop-adaptation">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="food-stock-loss">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-forest-area">
      <value value="0"/>
      <value value="15"/>
      <value value="30"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perennial-cover-agroforestry">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diverse-households">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-recovery-delay">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LER">
      <value value="1.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disruption-shield">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pop-density">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario_1_120y_4SB_" repetitions="2000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt;= 120</exitCondition>
    <metric>forest-cover</metric>
    <metric>forest-biodiversity</metric>
    <metric>edible-species-diversity</metric>
    <metric>mean [achieved-yield] of patches with [land-use = "S&amp;B"] ; yield</metric>
    <metric>(1 - (mean [achieved-yield] of patches with [land-use = "S&amp;B"]) / (mean [expected-yield] of patches with [land-use = "S&amp;B"])) * 100; harvest loss</metric>
    <metric>mean [production] of turtles / 3504</metric>
    <metric>mean [energy-per-capita] of turtles / 876</metric>
    <metric>mean [food-groups] of turtles * 10 ; diet diversity</metric>
    <metric>count turtles * 4 / 5.073 ; pop density</metric>
    <metric>mean [food-stock] of turtles / 4 / 876 ; indiv needs covered by food stocks</metric>
    <metric>876 / mean [energy-from-forest] of turtles / 4 ; % needs covered by wild foods</metric>
    <metric>sum [production] of turtles</metric>
    <metric>count patches with [land-use = "fallow"] / count patches with [land-use != "forest"] ; % fallows</metric>
    <metric>count patches with [land-use = "fallow"] / count patches with [land-use = "S&amp;B"] ; ratio of fallowed to cultivated plots</metric>
    <metric>count patches with [land-use = "degraded"] / count patches with [land-use != "forest"] ; % ag plots degraded</metric>
    <metric>mean [degradation] of patches with [land-use != "forest"] * 10 ; degradation of agricultural patches</metric>
    <metric>mean [perennial-cover] of patches with [land-use = "fallow"] ; average fallow state</metric>
    <metric>mean-tree-cover ; vegetation cover of non-forest patches</metric>
    <metric>mean [soil-biodiversity] of patches with [land-use = "fallow"]</metric>
    <metric>mean [soil-fertility] of patches with [land-use = "S&amp;B"]</metric>
    <metric>mean [soil-fertility] of patches with [land-use = "S&amp;B" or land-use = "degraded"]</metric>
    <metric>mean [climate-regulation] of patches with [land-use = "S&amp;B"]</metric>
    <metric>mean [production-target] of turtles</metric>
    <metric>sum [cultivated-plots] of turtles</metric>
    <enumeratedValueSet variable="improved-fallow-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target-surplus">
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disruption-level">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="species-loss-per-foraging">
      <value value="0.075"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degradation-pace">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-yield-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-duration">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forest-BD-loss-threshold">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="production-diversity-S&amp;B">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-bd-agroforestry">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S&amp;B-households">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deter-disruption-level">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crop-diversity-agroforestry">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-fallow-state">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forest-pollination">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-recovery-pace">
      <value value="0.065"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="foraging-intensity">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paddy-households">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochastic-disruption">
      <value value="&quot;yes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="species-recovery-pace">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="foraging-quota">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-forest-cover">
      <value value="7.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-years-cultivated">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tree-regrowth-pace">
      <value value="0.065"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tree-cover-after-clearing">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-patch-if-starving">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-loss-pace">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-1%-forest">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-drop-adaptation">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="food-stock-loss">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-forest-area">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perennial-cover-agroforestry">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diverse-households">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-recovery-delay">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LER">
      <value value="1.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disruption-shield">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pop-density">
      <value value="5000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario_3_120y_36pd_forest0-40_desired_0" repetitions="4000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt;= 120</exitCondition>
    <metric>forest-cover</metric>
    <metric>forest-biodiversity</metric>
    <metric>edible-species-diversity</metric>
    <metric>mean [achieved-yield] of patches with [land-use = "paddy"] ; yield</metric>
    <metric>mean [expected-yield] of patches with [land-use = "paddy"] ; divide achieved yield by expected yield for harvest loss</metric>
    <metric>mean [production] of turtles / 3504</metric>
    <metric>mean [energy-per-capita] of turtles / 876</metric>
    <metric>mean [food-groups] of turtles * 10 ; diet diversity</metric>
    <metric>count turtles * 4 / 5.073 ; pop density</metric>
    <metric>mean [food-stock] of turtles / 4 / 876 ; indiv needs covered by food stocks</metric>
    <metric>876 / mean [energy-from-forest] of turtles / 4 ; % needs covered by wild foods</metric>
    <metric>sum [production] of turtles</metric>
    <metric>mean [production-target] of turtles</metric>
    <metric>sum [cultivated-plots] of turtles</metric>
    <metric>mean [climate-regulation] of patches with [land-use = "paddy"]</metric>
    <enumeratedValueSet variable="improved-fallow-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target-surplus">
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disruption-level">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="species-loss-per-foraging">
      <value value="0.075"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degradation-pace">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-yield-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-duration">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forest-BD-loss-threshold">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="production-diversity-S&amp;B">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-bd-agroforestry">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S&amp;B-households">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deter-disruption-level">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crop-diversity-agroforestry">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-fallow-state">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forest-pollination">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-recovery-pace">
      <value value="0.065"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="foraging-intensity">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paddy-households">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochastic-disruption">
      <value value="&quot;yes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="species-recovery-pace">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="foraging-quota">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-forest-cover">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-years-cultivated">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tree-regrowth-pace">
      <value value="0.065"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tree-cover-after-clearing">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-patch-if-starving">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-loss-pace">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-1%-forest">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-drop-adaptation">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="food-stock-loss">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-forest-area">
      <value value="0"/>
      <value value="15"/>
      <value value="30"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perennial-cover-agroforestry">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diverse-households">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-recovery-delay">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LER">
      <value value="1.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disruption-shield">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pop-density">
      <value value="5000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Scenario_3_120y_64div_forest0-40_desired_0" repetitions="2000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks &gt;= 120</exitCondition>
    <metric>forest-cover</metric>
    <metric>forest-biodiversity</metric>
    <metric>edible-species-diversity</metric>
    <metric>mean [achieved-yield] of patches with [land-use != "forest"] ; yield</metric>
    <metric>mean [expected-yield] of patches with [land-use != "forest"] ; divide achieved yield by expected yield for harvest loss</metric>
    <metric>mean [production] of turtles / 3504</metric>
    <metric>mean [energy-per-capita] of turtles / 876</metric>
    <metric>mean [food-groups] of turtles * 10 ; diet diversity</metric>
    <metric>count turtles * 4 / 5.073 ; pop density</metric>
    <metric>mean [food-stock] of turtles / 4 / 876 ; indiv needs covered by food stocks</metric>
    <metric>876 / mean [energy-from-forest] of turtles / 4 ; % needs covered by wild foods</metric>
    <metric>sum [production] of turtles</metric>
    <metric>mean [production-target] of turtles</metric>
    <metric>sum [cultivated-plots] of turtles</metric>
    <metric>mean [soil-biodiversity] of patches with [land-use != "forest"]</metric>
    <metric>mean [soil-fertility] of patches with [land-use != "forest"]</metric>
    <metric>mean [climate-regulation] of patches with [land-use != "forest"]</metric>
    <enumeratedValueSet variable="improved-fallow-rate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="target-surplus">
      <value value="1.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disruption-level">
      <value value="60"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="species-loss-per-foraging">
      <value value="0.075"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="degradation-pace">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="potential-yield-weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulation-duration">
      <value value="120"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forest-BD-loss-threshold">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="production-diversity-S&amp;B">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-bd-agroforestry">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S&amp;B-households">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deter-disruption-level">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="crop-diversity-agroforestry">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-fallow-state">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forest-pollination">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-recovery-pace">
      <value value="0.065"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="foraging-intensity">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="paddy-households">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stochastic-disruption">
      <value value="&quot;yes&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="species-recovery-pace">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="foraging-quota">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-forest-cover">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desired-years-cultivated">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tree-regrowth-pace">
      <value value="0.065"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tree-cover-after-clearing">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="clear-patch-if-starving">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-loss-pace">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="energy-from-1%-forest">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="yield-drop-adaptation">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="food-stock-loss">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minimum-forest-area">
      <value value="0"/>
      <value value="15"/>
      <value value="30"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perennial-cover-agroforestry">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="diverse-households">
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soil-BD-recovery-delay">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="LER">
      <value value="1.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disruption-shield">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-pop-density">
      <value value="5000"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
