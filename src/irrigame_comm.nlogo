turtles-own [	
  inv ; Investment in the public infrastructure
  inv-past ; Investment in last round
	earnings ; Earnings in this round
	watercol ; Water collected
	gate ; Gate open (=1) or closed (=0)
	harvest ; amount of tokens earned from getting water to your field
  strategy ; = 0 random; = 1 selfish ; = 2 cooperative strategy
  turnoff ; in social values model - amount of water to be collected that is expected to maximize utility
			]
globals [	
  done ; boolean to define whether the batch of runs is finished (used for behaviorsearch)
	infrastructure ; level of public infrastructure
	capacity ; capacity of the infrastructure (amount of water that can be processed per second)
	watersupply ; supply of water per second
	totalearnings ; amount of earnings by group
	totalharvest ; amount of earnings by water collection by group
	actualwatersupply ; actual water supply top the system (which is lower than water supply if capacity is lower than supply)
	gini-inv ; gini of investment decisions
	gini-ext ; gini of extraction of water
	maintenance ; the amount of investment needed to keep the infrastructure at a capacity of 30 units of water (= water supply) 
  sec ; secpnd in game collecting water
  limcom ; boolean that defines whether agents have limited communication of not
  pastinfrastructure ; infrastructure level of previous round
  inf-eff-F inf-eff-L ; infrastructure levels for limited and full communication
  inv-A-F inv-B-F inv-C-F inv-D-F inv-E-F ; data on investment levels per position with full communication
  ext-A-F ext-B-F ext-C-F ext-D-F ext-E-F ; data on extraction levels per position with full communication
  inv-A-L inv-B-L inv-C-L inv-D-L inv-E-L ; data on investment levels per position with limited communication
  ext-A-L ext-B-L ext-C-L ext-D-L ext-E-L ; data on extraction levels per position with limited communication
  gini-inv-F gini-inv-L ; data for gini coefficient investments for two treatments
  gini-ext-F gini-ext-L ; data for gini coefficient extractions for two treatments
  data-inf ; data infrastructure levels
  data-gini-inv data-gini-ext ; data gini investments and extractions
  data-inv-A data-inv-B data-inv-C data-inv-D data-inv-E ; data investments levels per position
  data-ext-A data-ext-B data-ext-C data-ext-D data-ext-E ; data extraction earnings per position

  list-inf ; list of simulated infrastructure levels
  list-gini-inv ; list of simulated gini coefficient investments
  list-gini-ext ; list of simulated gini coefficient extractions
  list-inv-A list-inv-B list-inv-C list-inv-D list-inv-E ; list of simulated investment levels per position
  list-ext-A list-ext-B list-ext-C list-ext-D list-ext-E ; list of simulated extraction levels per position
  inv-A inv-B inv-C inv-D inv-E ; average simulated investment level for a position
  ext-A ext-B ext-C ext-D ext-E ; average simulated extraction level for a position
  list-change ; list of changes of investments between rounds
  list-data-change-L ; data of change frequencies for limited communication experiments
  list-data-change-F ; data of change frequencies for full communication experiments
  list-data-change ; data of change frequencies for two treatments together

  metric-inv ; squared differences between data and simulation
  metric-ext
  metric-inf
  metric-change
  metric-gini-inv
  metric-gini-ext
  metric-mult

  s-metric-inv ; average for two treatments for the squared differences between data and simulation
  s-metric-ext 
  s-metric-inf 
  s-metric-change 
  s-metric-gini-inv 
  s-metric-gini-ext 
  
  decline-lv-inf
  decline-hv-inf
  supply-lv-wa
  supply-hv-wa
		]

to setup
  clear-all
  set metric-mult 0
  crt 5 
  set done 0
  set limcom true
  ; treatments
  set decline-lv-inf (list 25 30 25 20 35 20 15 30 30 25)
  set decline-hv-inf (list 25 10 10 80 10 5 10 80 10 10)
  set supply-lv-wa (list 27 31 26 35 33 28 29 25 34 32)
  set supply-hv-wa (list 27 31 22 36 25 32 38 21 29 39)
  ; data from experiments
  set inf-eff-F (list 92.95 83.38 79.14 78.43 78.43 74.19 73.76 74.00 74.24 72.57)
  set inf-eff-L (list 96.87 90.43 87.04 84.09 82.52 80.61 79.52 78.04 78.35 76.09)
  set inv-A-F (list 3.81 3.43 4.81 5.57 5.38 4.71 5.19 5.86 5.10 5.24)
  set inv-B-F (list 4.62 3.52 4.43 4.81 4.95 4.19 4.90 5.76 5.38 5.10)
  set inv-C-F (list 4.05 3.38 4.48 5.24 5.24 4.76 5.48 5.14 5.86 5.33)
  set inv-D-F (list 2.81 2.76 4.00 4.71 4.95 3.90 5.05 4.71 4.57 3.90)
  set inv-E-F (list 3.19 2.86 3.71 4.95 5.33 4.67 4.67 4.14 4.33 3.76)
  set ext-A-F (list 12.05 11.71 10.00 11.52 10.33 11.05 11.24 10.29 10.24 11.14)
  set ext-B-F (list 11.86 12.48 11.90 10.14 12.33 13.29 9.57 11.43 10.24 11.90)
  set ext-C-F (list 11.76 12.33 10.48 12.29 12.14 11.62 10.05 10.10 12.67 10.76)
  set ext-D-F (list 8.71 8.14 7.71 10.19 9.14 9.43 8.90 9.33 9.67 8.05)
  set ext-E-F (list 4.67 6.95 7.24 7.19 7.76 7.24 9.10 7.14 7.95 6.71)
  set inv-A-L (list 5.00 4.04 5.04 5.17 6.09 6.39 6.26 5.74 5.70 5.17)
  set inv-B-L (list 5.83 5.30 5.43 5.74 6.48 5.70 6.30 6.22 6.30 5.78)
  set inv-C-L (list 4.87 4.43 5.04 5.26 5.35 5.09 5.91 5.65 5.91 5.13)
  set inv-D-L (list 4.65 3.09 3.65 3.57 3.48 3.52 3.96 3.87 4.35 3.30)
  set inv-E-L (list 4.39 2.65 3.00 3.04 2.43 2.96 2.09 2.22 2.00 2.30)
  set ext-A-L (list 18.52 17.22 17.96 17.61 16.87 16.61 17.00 14.70 14.96 15.30)
  set ext-B-L (list 18.35 17.00 17.00 16.35 14.87 14.91 15.52 13.48 13.52 12.39)
  set ext-C-L (list 9.83 11.39 12.30 12.22 13.78 12.13 11.04 12.04 13.57 12.78)
  set ext-D-L (list 2.52 4.22 3.57 4.13 4.78 5.30 6.39 6.83 7.26 7.48)
  set ext-E-L (list 0.09 0.70 0.30 0.61 1.57 1.70 1.17 2.52 1.96 3.17)
  set gini-inv-F (list 0.156 0.179 0.095 0.081 0.067 0.131 0.120 0.089 0.104 0.134)
  set gini-inv-L (list 0.267 0.352 0.269 0.256 0.244 0.268 0.256 0.224 0.259 0.303)
  set gini-ext-F (list 0.291 0.283 0.280 0.223 0.249 0.241 0.264 0.182 0.219 0.257)
  set gini-ext-L (list 0.456 0.426 0.421 0.417 0.392 0.380 0.377 0.372 0.333 0.327)
  setupcleardata
  set list-inf (list 0 0 0 0 0 0 0 0 0 0)
  set list-gini-inv (list 0 0 0 0 0 0 0 0 0 0)
  set list-gini-ext (list 0 0 0 0 0 0 0 0 0 0)
  set list-inv-A (list 0 0 0 0 0 0 0 0 0 0)
  set list-inv-B (list 0 0 0 0 0 0 0 0 0 0)
  set list-inv-C (list 0 0 0 0 0 0 0 0 0 0)
  set list-inv-D (list 0 0 0 0 0 0 0 0 0 0)
  set list-inv-E (list 0 0 0 0 0 0 0 0 0 0)
  set list-ext-A (list 0 0 0 0 0 0 0 0 0 0)
  set list-ext-B (list 0 0 0 0 0 0 0 0 0 0)
  set list-ext-C (list 0 0 0 0 0 0 0 0 0 0)
  set list-ext-D (list 0 0 0 0 0 0 0 0 0 0)
  set list-ext-E (list 0 0 0 0 0 0 0 0 0 0)
  set list-change (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  set list-data-change-L (list 0.004830918 0.000966184 0 0.004830918 0.001932367 0.025120773 0.021256039 0.043478261 0.066666667 0.127536232 0.403864734 0.142995169 0.07826087 0.036714976 0.015458937 0.011594203 0.004830918 0.004830918 0.001932367 0 0.002898551)
  set list-data-change-F (list 0.001058201 0.001058201 0.002116402 0.008465608 0.006349206 0.015873016 0.028571429 0.034920635 0.067724868 0.106878307 0.397883598 0.138624339 0.070899471 0.050793651 0.033862434 0.015873016 0.007407407 0.007407407 0.001058201 0.001058201 0.002116402)
    
  set metric-inv 0 
  set metric-ext 0
  set metric-inf 0
  set metric-change 0
  set metric-gini-inv 0
  set metric-gini-ext 0
  set metric-mult 0
  set limcom false
end

to setupcleardata
  ask turtles
  [
    set inv 0 set earnings 0 set watercol 0 set gate 0
  ]
  set infrastructure 100
  set capacity 0
  set watersupply 30
  if scenario = "random" [ask turtles [set strategy 0]]
  if scenario = "selfish" [ask turtles [set strategy 1]]
  if scenario = "cooperative" [ask turtles [set strategy 2]]
  if scenario = "mix" [
    ask turtles [
      let randomnumber random-float 1
      ifelse randomnumber <= sharerandom [set strategy 0]
      [ifelse randomnumber <= (sharerandom + shareselfish) [ set strategy 1][set strategy 2]
      ]
    ]
  ]
  reset-ticks
end

  
to batch ; batch is used for calibration where 1000 simulations for the same parameter setting are used to calculate the fit
  set metric-inv 0
  set metric-ext 0
  set metric-inf 0
  set metric-change 0
  set metric-gini-inv 0
  set metric-gini-ext 0
  if sharerandom + shareselfish > 1 [stop] ; One cannot have more than 100%
  let iter 0
  set metric-mult 0
  set limcom true
  while [iter < 1000]
  [
   setupcleardata
   while [ticks < 10]
   [ 
    go
   ] 
    set iter iter + 1
  ]
  calcmetrics
  
  set list-inf (list 0 0 0 0 0 0 0 0 0 0)
  set list-gini-inv (list 0 0 0 0 0 0 0 0 0 0)
  set list-gini-ext (list 0 0 0 0 0 0 0 0 0 0)
  set list-inv-A (list 0 0 0 0 0 0 0 0 0 0)
  set list-inv-B (list 0 0 0 0 0 0 0 0 0 0)
  set list-inv-C (list 0 0 0 0 0 0 0 0 0 0)
  set list-inv-D (list 0 0 0 0 0 0 0 0 0 0)
  set list-inv-E (list 0 0 0 0 0 0 0 0 0 0)
  set list-ext-A (list 0 0 0 0 0 0 0 0 0 0)
  set list-ext-B (list 0 0 0 0 0 0 0 0 0 0)
  set list-ext-C (list 0 0 0 0 0 0 0 0 0 0)
  set list-ext-D (list 0 0 0 0 0 0 0 0 0 0)
  set list-ext-E (list 0 0 0 0 0 0 0 0 0 0)
  set list-change (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

  set s-metric-inv metric-inv
  set s-metric-ext metric-ext
  set s-metric-inf metric-inf
  set s-metric-change metric-change
  set s-metric-gini-inv metric-gini-inv
  set s-metric-gini-ext metric-gini-ext
  set metric-inv 0
  set metric-ext 0
  set metric-inf 0
  set metric-change 0
  set metric-gini-inv 0
  set metric-gini-ext 0
  
  set iter 0
  set limcom true ; since we calculate the fit for two different treatments we now run the 1000 times the other treatment
  while [iter < 1000]
  [
   setupcleardata
   while [ticks < 10]
   [  
    go
   ] 
    set iter iter + 1
  ]
  calcmetrics
  set metric-mult metric-mult / 2
  set s-metric-inv (s-metric-inv + metric-inv) / 2
  set s-metric-ext (s-metric-ext + metric-ext) / 2
  set s-metric-inf (s-metric-inf + metric-inf) / 2
  set s-metric-change (s-metric-change + metric-change) / 2
  set s-metric-gini-inv (s-metric-gini-inv + metric-gini-inv) / 2
  set s-metric-gini-ext (s-metric-gini-ext + metric-gini-ext) / 2
  set done 1
end

to go
  if ticks = 20 [stop] ; 10 rounds in the experiment
  if ticks = 10 [set infrastructure 100]
  createdata

  ifelse ticks >= 10 and (Variability = "LV-inf" or Variability = "HV-inf")
  [
    if Variability = "LV-inf" [
      set infrastructure infrastructure - item (ticks - 10) decline-lv-inf
    ]
    if Variability = "HV-inf" [
      set infrastructure infrastructure - item (ticks - 10) decline-hv-inf
    ]
  ][
    set infrastructure infrastructure - 25 ; 25 is the round based reduction of the infrastructure (which cannot go below 0)
  ]
  if infrastructure < 0 [set infrastructure 0]
  set maintenance 66 - infrastructure 
  if maintenance > 50 [set maintenance 50] ; if more than 50 units is needed for repair, the maximum investment is still 50
  
  ; update expectations from communication and extractions
  ask turtles [
    ifelse ticks = 0 [
    ][
      let score 0
      if mean [harvest] of turtles > 0 [
          ifelse (harvest / mean [harvest] of turtles) < 1 [set score (harvest / mean [harvest] of turtles)][set score 1]
      ]
    ]   
  ]

  invest
  
   
  set sec 0

  ifelse ticks >= 10 and (Variability = "LV-wa" or Variability = "HV-wa")
  [
    if Variability = "LV-wa" [set watersupply item (ticks - 10) supply-lv-wa
    ]
    if Variability = "HV-wa" [set watersupply item (ticks - 10) supply-hv-wa
    ]
  ][set watersupply 30]
  
  ask turtles [set watercol 0]
  ifelse capacity < watersupply [
		set actualwatersupply capacity
	]
	[	
		set actualwatersupply watersupply
	]

  ask turtles [set gate 1] ; initially all gates are open
  while [sec < 50]
  [ 
    set watersupply actualwatersupply
    getwater
    set sec sec + 1
  ]
  ask turtles [
    set harvest calharvest watercol
    set earnings earnings + harvest
  ]
  set totalearnings sum [earnings] of turtles
  set totalharvest sum [harvest] of turtles
  ; after the round calculate the actual utilities
   
  ; calculate the metrics
  calcgini
  
  set list-inf replace-item ticks list-inf (item ticks list-inf + infrastructure)
  set list-gini-inv replace-item ticks list-gini-inv (item ticks list-gini-inv + gini-inv)
  set list-gini-ext replace-item ticks list-gini-ext (item ticks list-gini-ext + gini-ext)
  set list-inv-A replace-item ticks list-inv-A (item ticks list-inv-A + [inv] of turtle 0)
  set list-inv-B replace-item ticks list-inv-B (item ticks list-inv-B + [inv] of turtle 1)
  set list-inv-C replace-item ticks list-inv-C (item ticks list-inv-C + [inv] of turtle 2)
  set list-inv-D replace-item ticks list-inv-D (item ticks list-inv-D + [inv] of turtle 3)
  set list-inv-E replace-item ticks list-inv-E (item ticks list-inv-E + [inv] of turtle 4)
  set list-ext-A replace-item ticks list-ext-A (item ticks list-ext-A + [harvest] of turtle 0)
  set list-ext-B replace-item ticks list-ext-B (item ticks list-ext-B + [harvest] of turtle 1)
  set list-ext-C replace-item ticks list-ext-C (item ticks list-ext-C + [harvest] of turtle 2)
  set list-ext-D replace-item ticks list-ext-D (item ticks list-ext-D + [harvest] of turtle 3)
  set list-ext-E replace-item ticks list-ext-E (item ticks list-ext-E + [harvest] of turtle 4)
  if ticks > 0 [
    let change 0
    ask turtles [
      set change inv - inv-past
      let tickstart 0
      if ticks > tickstart [
      if (change = (0 - 10)) [set list-change replace-item 0 list-change (item 0 list-change + 1)]
      if (change = (0 - 9)) [set list-change replace-item 1 list-change (item 1 list-change + 1)]
      if (change = (0 - 8)) [set list-change replace-item 2 list-change (item 2 list-change + 1)]
      if (change = (0 - 7)) [set list-change replace-item 3 list-change (item 3 list-change + 1)]
      if (change = (0 - 6)) [set list-change replace-item 4 list-change (item 4 list-change + 1)]
      if (change = (0 - 5)) [set list-change replace-item 5 list-change (item 5 list-change + 1)]
      if (change = (0 - 4)) [set list-change replace-item 6 list-change (item 6 list-change + 1)]
      if (change = (0 - 3)) [set list-change replace-item 7 list-change (item 7 list-change + 1)]
      if (change = (0 - 2)) [set list-change replace-item 8 list-change (item 8 list-change + 1)]
      if (change = (0 - 1)) [set list-change replace-item 9 list-change (item 9 list-change + 1)]
      if (change = (0)) [set list-change replace-item 10 list-change (item 10 list-change + 1)]
      if (change = 1) [set list-change replace-item 11 list-change (item 11 list-change + 1)]
      if (change = 2) [set list-change replace-item 12 list-change (item 12 list-change + 1)]
      if (change = 3) [set list-change replace-item 13 list-change (item 13 list-change + 1)]
      if (change = 4) [set list-change replace-item 14 list-change (item 14 list-change + 1)]
      if (change = 5) [set list-change replace-item 15 list-change (item 15 list-change + 1)]
      if (change = 6) [set list-change replace-item 16 list-change (item 16 list-change + 1)]
      if (change = 7) [set list-change replace-item 17 list-change (item 17 list-change + 1)]
      if (change = 8) [set list-change replace-item 18 list-change (item 18 list-change + 1)]
      if (change = 9) [set list-change replace-item 19 list-change (item 19 list-change + 1)]
      if (change = 10) [set list-change replace-item 20 list-change (item 20 list-change + 1)]
      ] 
    ] 
  ] 

  tick
end

to invest ; this is how investment decisions are made
  set pastinfrastructure infrastructure
  ask turtles [
    set inv-past inv      
    
    if strategy = 0 [set inv random 11]
    if strategy = 1 [
      set inv 0
      if who = 0 [
        let sizeinvestment 0
        let potearnings 0
        if infrastructure < 46 [if (46 - infrastructure) <= 10 [set sizeinvestment 46 - infrastructure set potearnings 10 - sizeinvestment]] ; one can calculate this is the optimal decision for selfish individuals who invest if they expect to get at least the amount out of it that they invest and assume upstream agents are also selfish and rational
        if infrastructure < 52 [if  (52 - infrastructure) <= 10 [
          if (20 - 52 + infrastructure) > potearnings  [set sizeinvestment 52 - infrastructure set potearnings 20 - sizeinvestment]]] 
        set inv sizeinvestment  
      ]
      if who = 1 [
        let sizeinvestment 0
        let potearnings 0
        if infrastructure < 56 [if (56 - infrastructure) <= 10 [set sizeinvestment 56 - infrastructure set potearnings 10 - sizeinvestment]]
        if infrastructure < 59 [if (59 - infrastructure) <= 10 [
          if (20 - 59 + infrastructure) > potearnings  [set sizeinvestment 59 - infrastructure set potearnings 20 - sizeinvestment]]] 
        set inv sizeinvestment 
      ]
      if who = 2 [
        let sizeinvestment 0
        let potearnings 0
        if infrastructure < 62 [if (62 - infrastructure) <= 10 [set sizeinvestment 62 - infrastructure set potearnings 10 - sizeinvestment]]
        if infrastructure < 66 [if (66 - infrastructure) <= 10 [if (20 - 66 + infrastructure) > potearnings  [set sizeinvestment 66 - infrastructure set potearnings 20 - sizeinvestment]]]
        set inv sizeinvestment 
      ]
      set inv inv + random-normal 0 stdev-random ; the trembling hand in investment decisions
      set inv int (inv)
      if inv < 0 [set inv 0]
      if inv > 10 [set inv 10]    
    ]
    
    
    if strategy = 2 [
      ifelse limcom [
        ifelse visioneffect [
       ifelse infrastructure < 66
       [
         if (who = 0 or who = 4) [set inv ceiling (maintenance / 2)]
         if (who = 1 or who = 2 or who = 3) [set inv ceiling (maintenance / 3)] 
       ][
         set inv 0
       ]][
        ifelse infrastructure < 66 [
          set inv ceiling (maintenance / 5)
        ]
        [
          set inv 0
        ]
       ]
      ][
        ifelse infrastructure < 66 [
          set inv ceiling (maintenance / 5)
        ]
        [
          set inv 0
        ]
      ]
      set inv inv + random-normal 0 stdev-random
      set inv int (inv)
      if inv < 0 [set inv 0]
      if inv > 10 [set inv 10]  
    ]
    
    set earnings 10 - inv
  ]
  set infrastructure infrastructure + sum [inv] of turtles
  if infrastructure > 100 [set infrastructure 100]
  calcapacity
end

to calcapacity ; calculating the capacity of a certain level of infrastructure
   set capacity 0
   ifelse infrastructure <= 45 [set capacity 0 ][
     ifelse infrastructure <= 51 [set capacity 5][
       ifelse infrastructure <= 55 [set capacity 10][
         ifelse infrastructure <= 58 [set capacity 15][
           ifelse infrastructure <= 61 [set capacity 20][
             ifelse infrastructure <= 65 [set capacity 25][
               ifelse infrastructure <= 70 [set capacity 30][
                 ifelse infrastructure <= 80 [set capacity 35][set capacity 40]]]]]]]]
end

to-report calharvest [water] ; calculating the earnings from collecting a certain amount of water
  let crops 0
  ifelse water < 150 [set crops 0 ][
    ifelse water <= 199 [set crops 1][
      ifelse water <= 249 [set crops 4][
        ifelse water <= 299 [set crops 10][
          ifelse water <= 349 [set crops 15][
            ifelse water <= 399 [set crops 18][
              ifelse water <= 499 [set crops 19][
                ifelse water <= 549 [set crops 20][
                  ifelse water <= 649 [set crops 19][
                    ifelse water <= 699 [set crops 18][
                      ifelse water <= 749 [set crops 15][
                        ifelse water <= 799 [set crops 10][ 
                          ifelse water <= 849 [set crops 4][
                            ifelse water <= 899 [set crops 1][set crops 0]]]]]]]]]]]]]]
  report crops
end

to getwater ; define when to open and close gates
  let i 0
  let totalwaterremaining watersupply * (50 - sec)
  while [i < 5]
  [
    ask turtle i [
      if strategy = 1 [
        ifelse watercol < 500 [set gate 1][set gate 0]
      ]
      if strategy = 2 [
        let equalshare ceiling (10 * watersupply)
        ifelse watercol < equalshare [set gate 1][set gate 0]
      ]
 
      if gate = 1 [
        ifelse watersupply >= 25 [
		  	  set watercol watercol + 25 set watersupply watersupply - 25
		    ][
			    set watercol watercol + watersupply set watersupply 0
		    ]
	    ]
    ]
    set i i + 1
  ]
end

to calcgini
  set gini-inv 0
  set gini-ext 0
 
  ifelse sum [inv] of turtles > 0 [
    set gini-inv (abs ([inv] of turtle 0 - [inv] of turtle 1) + abs ([inv] of turtle 0 - [inv] of turtle 2) + abs ([inv] of turtle 0 - [inv] of turtle 3) + abs ([inv] of turtle 0 - [inv] of turtle 4) + abs ([inv] of turtle 1 - [inv] of turtle 2) + abs ([inv] of turtle 1 - [inv] of turtle 3) + abs ([inv] of turtle 1 - [inv] of turtle 4) + abs ([inv] of turtle 2 - [inv] of turtle 3) + abs ([inv] of turtle 2 - [inv] of turtle 4) + abs ([inv] of turtle 3 - [inv] of turtle 4)) / (5 * sum [inv] of turtles)
   ][
    set gini-inv 0
   ]
  
  ifelse sum [harvest] of turtles > 0 [
    set gini-ext (abs ([harvest] of turtle 0 - [harvest] of turtle 1) + abs ([harvest] of turtle 0 - [harvest] of turtle 2) + abs ([harvest] of turtle 0 - [harvest] of turtle 3) + abs ([harvest] of turtle 0 - [harvest] of turtle 4) + abs ([harvest] of turtle 1 - [harvest] of turtle 2) + abs ([harvest] of turtle 1 - [harvest] of turtle 3) + abs ([harvest] of turtle 1 - [harvest] of turtle 4) + abs ([harvest] of turtle 2 - [harvest] of turtle 3) + abs ([harvest] of turtle 2 - [harvest] of turtle 4) + abs ([harvest] of turtle 3 - [harvest] of turtle 4)) / (5 * sum [harvest] of turtles)
  ][
    set gini-ext 0
  ]
end
  
  to createdata
      ifelse limcom  [
        if Variability = "LV-inf" []
        if Variability = "HV-inf" []
        if Variability = "LV-wa" []
        if Variability = "HV-wa" []
    set data-inf item ticks inf-eff-L
    set data-gini-inv item ticks gini-inv-L
    set data-gini-ext item ticks gini-ext-L
    set data-inv-A item ticks inv-A-L
    set data-inv-B item ticks inv-B-L
    set data-inv-C item ticks inv-C-L
    set data-inv-D item ticks inv-D-L
    set data-inv-E item ticks inv-E-L
    set data-ext-A item ticks ext-A-L
    set data-ext-B item ticks ext-B-L
    set data-ext-C item ticks ext-C-L
    set data-ext-D item ticks ext-D-L
    set data-ext-E item ticks ext-E-L
    set list-data-change list-data-change-L
  ][
    if Variability = "LV-inf" []
    if Variability = "HV-inf" []
    if Variability = "LV-wa" []
    if Variability = "HV-wa" []
    set data-inf item ticks inf-eff-F
    set data-gini-inv item ticks gini-inv-F
    set data-gini-ext item ticks gini-ext-F
    set data-inv-A item ticks inv-A-F
    set data-inv-B item ticks inv-B-F
    set data-inv-C item ticks inv-C-F
    set data-inv-D item ticks inv-D-F
    set data-inv-E item ticks inv-E-F
    set data-ext-A item ticks ext-A-F
    set data-ext-B item ticks ext-B-F
    set data-ext-C item ticks ext-C-F
    set data-ext-D item ticks ext-D-F
    set data-ext-E item ticks ext-E-F
    set list-data-change list-data-change-F
  ] 
  end

to calcmetrics ; calculating the metrics to evaluate the fit between data and simulation
   let i 0
   while [i <= 20]
   [
     set list-change replace-item i list-change (item i list-change / (45000))
     set i i + 1
   ]
      
   clear-all-plots 
   reset-ticks

   while [ticks < 10]
   [
     createdata
     set infrastructure (item ticks list-inf / 1000)
     set gini-inv (item ticks list-gini-inv / 1000)
     set gini-ext (item ticks list-gini-ext / 1000)
     set inv-A (item ticks list-inv-A / 1000)
     set inv-B (item ticks list-inv-B / 1000)
     set inv-C (item ticks list-inv-C / 1000)
     set inv-D (item ticks list-inv-D / 1000)
     set inv-E (item ticks list-inv-E / 1000)
     set ext-A (item ticks list-ext-A / 1000)
     set ext-B (item ticks list-ext-B / 1000)
     set ext-C (item ticks list-ext-C / 1000)
     set ext-D (item ticks list-ext-D / 1000)
     set ext-E (item ticks list-ext-E / 1000)

     if ticks > 0 [
       set metric-inv metric-inv + (inv-A - data-inv-A) ^ 2 + (inv-B - data-inv-B) ^ 2 + (inv-C - data-inv-C) ^ 2 + (inv-D - data-inv-D) ^ 2 + (inv-E - data-inv-E) ^ 2
       set metric-ext metric-ext + (ext-A - data-ext-A) ^ 2 + (ext-B - data-ext-B) ^ 2 + (ext-C - data-ext-C) ^ 2 + (ext-D - data-ext-D) ^ 2 + (ext-E - data-ext-E) ^ 2
       set metric-inf metric-inf + (infrastructure - data-inf) ^ 2

       set metric-gini-inv metric-gini-inv + (gini-inv - data-gini-inv) ^ 2
       set metric-gini-ext metric-gini-ext + (gini-ext - data-gini-ext) ^ 2
     ]
     tick
   ]
   set metric-change metric-change +  (item 0 list-data-change - item 0 list-change) ^ 2 + (item 1 list-data-change - item 1 list-change) ^ 2 + (item 2 list-data-change - item 2 list-change) ^ 2 
     + (item 3 list-data-change - item 3 list-change) ^ 2 + (item 4 list-data-change - item 4 list-change) ^ 2 + (item 5 list-data-change - item 5 list-change) ^ 2 + (item 6 list-data-change - item 6 list-change) ^ 2 
     + (item 7 list-data-change - item 7 list-change) ^ 2 + (item 8 list-data-change - item 8 list-change) ^ 2 + (item 9 list-data-change - item 9 list-change) ^ 2 + (item 10 list-data-change - item 10 list-change) ^ 2 
     + (item 11 list-data-change - item 11 list-change) ^ 2 + (item 12 list-data-change - item 12 list-change) ^ 2 + (item 13 list-data-change - item 13 list-change) ^ 2 + (item 14 list-data-change - item 14 list-change) ^ 2 
     + (item 15 list-data-change - item 15 list-change) ^ 2 + (item 16 list-data-change - item 16 list-change) ^ 2 + (item 17 list-data-change - item 17 list-change) ^ 2 + (item 18 list-data-change - item 18 list-change) ^ 2 
     + (item 19 list-data-change - item 19 list-change) ^ 2 + (item 20 list-data-change - item 20 list-change) ^ 2 
     
   set metric-inv ((metric-inv / 50) ^ 0.5) / 10 
   set metric-ext ((metric-ext / 50 ) ^ 0.5) / 20
   set metric-inf ((metric-inf / 10) ^ 0.5) / 100 
   set metric-change ((metric-change / 210) ^ 0.5) 
   set metric-gini-inv ((metric-gini-inv / 10) ^ 0.5) / 0.8 
   set metric-gini-ext ((metric-gini-ext / 10) ^ 0.5) / 0.8 
   
   set metric-mult metric-mult + ((1 - metric-inv) * (1 - metric-ext) * (1 - metric-inf) * (1 - metric-change)  * (1 - metric-gini-inv) * (1 - metric-gini-ext))
end  
@#$#@#$#@
GRAPHICS-WINDOW
415
10
660
195
1
1
51.33333333333334
1
10
1
1
1
0
1
1
1
-1
1
-1
1
0
0
1
ticks
30.0

BUTTON
71
10
134
43
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
136
10
199
43
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

PLOT
602
10
1019
294
infrastructure
NIL
NIL
1.0
20.0
0.0
100.0
false
false
"" ""
PENS
"infrastructure" 1.0 0 -13345367 true "" "plot infrastructure"
"data" 1.0 0 -16777216 true "" "plot data-inf"

CHOOSER
203
10
376
55
scenario
scenario
"selfish" "cooperative" "random" "mix"
3

PLOT
1032
10
1475
295
Gini
NIL
NIL
1.0
20.0
0.0
0.8
false
true
"" ""
PENS
"inv" 1.0 0 -817084 true "" "plot gini-inv"
"extract" 1.0 0 -5298144 true "" "plot gini-ext"
"data-inv" 1.0 0 -7500403 true "" "plot data-gini-inv"
"data-ext" 1.0 0 -16777216 true "" "plot data-gini-ext"

BUTTON
137
46
200
79
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

BUTTON
71
45
134
78
NIL
batch
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
602
296
1018
521
Investment per position
NIL
NIL
1.0
20.0
0.0
10.0
false
false
"" ""
PENS
"A" 1.0 0 -16777216 true "" "plot inv-A"
"B" 1.0 0 -7500403 true "" "plot inv-B"
"C" 1.0 0 -2674135 true "" "plot inv-C"
"D" 1.0 0 -955883 true "" "plot inv-D"
"E" 1.0 0 -6459832 true "" "plot inv-E"

PLOT
1028
299
1476
528
Extraction per position
NIL
NIL
1.0
20.0
0.0
20.0
false
false
"" ""
PENS
"A" 1.0 0 -16777216 true "" "plot ext-A"
"B" 1.0 0 -7500403 true "" "plot ext-B"
"C" 1.0 0 -2674135 true "" "plot ext-C"
"D" 1.0 0 -955883 true "" "plot ext-D"
"E" 1.0 0 -6459832 true "" "plot ext-E"

SLIDER
206
61
378
94
sharerandom
sharerandom
0
1
0.06
0.01
1
NIL
HORIZONTAL

SLIDER
205
97
377
130
shareselfish
shareselfish
0
1
0.07
0.01
1
NIL
HORIZONTAL

SLIDER
218
192
390
225
stdev-random
stdev-random
0
10
1.02
0.01
1
NIL
HORIZONTAL

SWITCH
22
117
142
150
visioneffect
visioneffect
0
1
-1000

CHOOSER
23
163
161
208
Variability
Variability
"LV-inf" "HV-inf" "LV-inf" "HV-inf"
0

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
NetLogo 5.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>batch</go>
    <exitCondition>done = 1</exitCondition>
    <metric>metric-mult</metric>
    <metric>(1 - s-metric-inv)</metric>
    <metric>(1 - s-metric-ext)</metric>
    <metric>(1 - s-metric-inf)</metric>
    <metric>(1 - s-metric-change)</metric>
    <metric>(1 - s-metric-gini-inv)</metric>
    <metric>(1 - s-metric-gini-ext)</metric>
    <enumeratedValueSet variable="gamma-cominv">
      <value value="0.32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-expC">
      <value value="0.79"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-ext">
      <value value="3.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gammawatercol">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gammaEC">
      <value value="327"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sharerandom" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="stdev-random">
      <value value="0.04"/>
    </enumeratedValueSet>
    <steppedValueSet variable="shareselfish" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="comfulscore">
      <value value="0.85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;mix&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comlimscore">
      <value value="0.57"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.54"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="psat">
      <value value="0.22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold1">
      <value value="13.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomseed">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>batch</go>
    <timeLimit steps="1"/>
    <metric>list-inf</metric>
    <metric>list-gini-inv</metric>
    <metric>list-gini-ext</metric>
    <metric>list-inv-A</metric>
    <metric>list-inv-B</metric>
    <metric>list-inv-C</metric>
    <metric>list-inv-D</metric>
    <metric>list-inv-E</metric>
    <metric>list-ext-A</metric>
    <metric>list-ext-B</metric>
    <metric>list-ext-C</metric>
    <metric>list-ext-D</metric>
    <metric>list-ext-E</metric>
    <metric>list-change</metric>
    <enumeratedValueSet variable="visioneffect">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last5rounds">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="62"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stdev-random">
      <value value="1.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-expC">
      <value value="0.93"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comlimscore">
      <value value="0.42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shareselfish">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;mix&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gammawatercol">
      <value value="6.57"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold5">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold3">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sharerandom">
      <value value="0.34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda-ext">
      <value value="0.09"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.82"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda">
      <value value="0.65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold1">
      <value value="15.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comfulscore">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold4">
      <value value="39.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.94"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gammaEC">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-ext">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="psat">
      <value value="0.22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda-inv">
      <value value="0.44"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-cominv">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold2">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>batch</go>
    <exitCondition>done = 1</exitCondition>
    <metric>metric-mult</metric>
    <metric>(1 - s-metric-inv)</metric>
    <metric>(1 - s-metric-ext)</metric>
    <metric>(1 - s-metric-inf)</metric>
    <metric>(1 - s-metric-change)</metric>
    <metric>(1 - s-metric-gini-inv)</metric>
    <metric>(1 - s-metric-gini-ext)</metric>
    <enumeratedValueSet variable="gamma-cominv">
      <value value="0.32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-expC">
      <value value="0.79"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-ext">
      <value value="3.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gammawatercol">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gammaEC">
      <value value="327"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sharerandom">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stdev-random">
      <value value="0.04"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shareselfish">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comfulscore">
      <value value="0.85"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;mix&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda">
      <value value="0.11"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comlimscore">
      <value value="0.57"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.97"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.54"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="psat">
      <value value="0.22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold1">
      <value value="13.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomseed">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment5" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>batch</go>
    <exitCondition>done = 1</exitCondition>
    <metric>metric-mult</metric>
    <metric>(1 - s-metric-inv)</metric>
    <metric>(1 - s-metric-ext)</metric>
    <metric>(1 - s-metric-inf)</metric>
    <metric>(1 - s-metric-change)</metric>
    <metric>(1 - s-metric-gini-inv)</metric>
    <metric>(1 - s-metric-gini-ext)</metric>
    <enumeratedValueSet variable="gamma-cominv">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-expC">
      <value value="0.93"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma-ext">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gammawatercol">
      <value value="6.57"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gammaEC">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sharerandom">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stdev-random">
      <value value="1.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shareselfish">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comfulscore">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;social-values&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda">
      <value value="0.65"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="comlimscore">
      <value value="0.42"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="alpha">
      <value value="0.94"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="beta">
      <value value="0.82"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="psat">
      <value value="0.22"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="threshold1">
      <value value="15.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mu">
      <value value="62"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda-inv">
      <value value="0.44"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lambda-ext">
      <value value="0.09"/>
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
