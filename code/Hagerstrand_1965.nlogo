;;:::::::::::::::::::::::::::::::::::::::  
;; Torsten Hägerstrand's Monte Carlo Approach to Diffusion
;;
;; Model replicated by Sean Bergin (sean.m.bergin@gmail.com) in 2012
;;:::::::::::::::::::::::::::::::::::::::

breed [person persons]
breed [barrier-endpoints barrier-endpoint]
undirected-link-breed [half-links half-link]
undirected-link-breed [barrier-links barrier-link]
undirected-link-breed [normal-links normal-link]
patches-own [farm-here?]
globals [isotropic-model? total-adopted patch-data]
turtles-own [innovation-adopted? moore-neighborhood link-ref]

to setup
  clear-all  
  ifelse Model-Type = "isotropic Isotropic Model" [ setup-isotropic-model]
  [ setup-anisotropic-model  ]
  reset-ticks
end

to go
;;:::::::::::::::::::::::::::::::::   
;; GO Isotropic Model of Diffusion
;;:::::::::::::::::::::::::::::::::  

  ifelse isotropic-model?[   
    ;;Each agent will spread an innovation during a timestamp
    let adopted-group person with [innovation-adopted?]
    set total-adopted  count adopted-group
    ask adopted-group [if innovation-adopted? [spread-innovation-isotropic set color red]]
    if count person = count adopted-group [stop]
  ]
;;:::::::::::::::::::::::::::::::::::  
;; GO  Anisotropic Model of Diffusion
;;:::::::::::::::::::::::::::::::::::
  [
    ;;Each agent with the innovation will spread the innovation during a timestamp
    let adopted-group person with [innovation-adopted?]
    set total-adopted  count adopted-group
    ask adopted-group [if innovation-adopted? [spread-innovation-anisotropic set color red ]] 
    set adopted-group person with [innovation-adopted?]
    set total-adopted  count adopted-group
    if count person = count adopted-group [stop]
  ]

  tick
end

to load-init-pop-from-file
  ;; code taken from the file input example in the Netlogo code example library
  
  let file user-file
  if ( file != false )
  [
    set patch-data []
    file-open file

    while [ not file-at-end? ]
      [ set patch-data sentence patch-data (list (list file-read file-read file-read)) ]


    file-close
  ]
  
;; The list is a list of three-tuples where the first item is the pxcor, the
;; second is the pycor, and the third is population. Ex. [ [ 0 0 5 ] [ 1 34 26 ] ... ]

  ifelse ( is-list? patch-data )
    [ foreach patch-data [ ask patch first ? item 1 ? [ sprout-person last ? [set color blue set size 0.25]] ] ]
    
    
    [ user-message "There was an error loading data from the file!" ]
  
   user-message "File loading successful!"
end

;;:::::::::::::::::::::::::::::::::::::::  
;; Setup Anisotropic Model of Diffusion
;;:::::::::::::::::::::::::::::::::::::::
to setup-anisotropic-model
  set isotropic-model? false
  set-default-shape person "person"
  set-default-shape barrier-endpoints "square"
  ask patches [ set pcolor green set farm-here? true] 
  format-sweeden-area
  create-sweeden-barriers
  ifelse read-data-from-file [load-init-pop-from-file]
  [  ;; Since we are unable to find Hagerstrand's Population per square data, the population is randomly distributed
  ask patches [if farm-here? [sprout-person (random max-farm-population) + 1  [set color blue set size 0.25]]] 
  ;; we assume that at least 5 people exist on a patch since it appears that this is true from an examination of the model
  ask patches [check-people-minimum]]
  
  ask person [set innovation-adopted? false set moore-neighborhood moore-offsets ]
  add-1929-adopters

end

;;:::::::::::::::::::::::::::::::::::::::  
;; Setup Isotropic Model of Diffusion
;;:::::::::::::::::::::::::::::::::::::::
to setup-isotropic-model

  set isotropic-model? true
  let isotropic-patch-set patches with [pxcor > 3 and pxcor < 9 and pycor > 3 and pycor < 9 ]
  ask isotropic-patch-set [set pcolor green sprout-person 30 [ set color blue set size 0.25 ]]
  ask turtles [set innovation-adopted? false]
   
;; In the isotropic model, one agent in the center of the grid begins the 
;; simulation after adopting an innovation
  let center-patch patches with [pxcor = 6 and pycor = 6] 
  let diffusion-source one-of person-on center-patch
  ask diffusion-source [set innovation-adopted? true]
  
end

to check-people-minimum
  if farm-here? [
    if count person-here < 5 [
      sprout-person 5  [set color blue set size 0.25]
    ] 
  ]
end

to spread-innovation-anisotropic
  let need-patch? true
  let q-sum 0
    while [need-patch?][
    let r-num ((random 100) + 1) / 100 
;; Randomly choose a nearby patch from the  moore neighborhood of 2 (5x5 grid)
    let patch-connection patches at-points moore-neighborhood with [farm-here? = true]
    set patch-connection one-of patch-connection   
    let other-x [pxcor] of patch-connection
    let other-y [pycor] of patch-connection
    let this-x [pxcor] of patch-here
    let this-y [pycor] of patch-here
;; get the proper q-value based on distance
    let q-val get-q-value this-x this-y other-x other-y
    
;; check to see if any barriers exist unless its the same patch
if this-x != other-x and this-y != other-y[
set q-val check-barriers q-val this-x this-y other-x other-y]
     if q-val != 0 [ 
        set q-sum q-sum + q-val
    
           if q-sum > r-num [
               let agent-connection one-of other person-on patch-connection
               if agent-connection != nobody [   
                   ask agent-connection [set innovation-adopted? true]
                   set need-patch? false]
       ]  
    ]
    ] 
  
end

to spread-innovation-isotropic
    let need-patch? true
    let p-sum 0
    while [need-patch?][
    let r-num ((random 100) + 1) / 100 
    
;; Randomly choose a nearby patch
    let patch-connection one-of patches with [pcolor = green]
    let other-x [pxcor] of patch-connection
    let other-y [pycor] of patch-connection
    let this-x [pxcor] of patch-here
    let this-y [pycor] of patch-here
;; get the proper p-value based on distance
    let p-val get-p-value this-x this-y other-x other-y
    set p-sum p-sum + p-val

    if p-sum > r-num [
          let agent-connection one-of other person-on patch-connection
          ask agent-connection [set innovation-adopted? true]
          set need-patch? false
       ]  
    ]
end

to-report check-barriers [q-val this-x this-y other-x other-y]
  let final-q-val q-val
  ;; create a link between current patch and other-patch to check for barriers
  let this-patch one-of patches with [pxcor = this-x and pycor = this-y]
  let other-patch one-of patches with [pxcor = other-x and pycor = other-y]
  let agent1 one-of person-on this-patch
  let agent2 one-of person-on other-patch  
  ask agent1 [create-normal-link-with agent2 [set color red set thickness 0.05]]
  let this-link normal-link [who] of agent1 [who] of agent2
;;check if link intersects a half-link barrier which reduces q by half
  ask half-links [let intersects? intersection self this-link
    if intersects? [set final-q-val (final-q-val / 2)]] 
;;check if link intersects a barrier-link, this makes it impossible for information to travel to a patch
  ask barrier-links [let intersects? intersection self this-link
    if intersects? [set final-q-val 0 ]
    ] 
  ask this-link [die]
  report final-q-val
end

to-report intersection [t1 t2] 
;; based upon the intersecting links example in Netlogo Model library by Uri Wilensky
  let m1 [tan (90 - link-heading)] of t1
  let m2 [tan (90 - link-heading)] of t2
  ;; treat parallel/collinear lines as non-intersecting
  if m1 = m2 [ report false ]
  ;; is t1 vertical? if so, swap the two turtles
  if abs m1 = tan 90
  [
    ifelse abs m2 = tan 90
      [ report false ]
      [report intersection t2 t1 ]
  ]
  ;; is t2 vertical? if so, handle specially
  if abs m2 = tan 90 [
     ;; represent t1 line in slope-intercept form (y=mx+c)
      let c1 [link-ycor - link-xcor * m1] of t1
      ;; t2 is vertical so we know x already
      let x [link-xcor] of t2
      ;; solve for y
      let y m1 * x + c1
      ;; check if intersection point lies on both segments
      if not [x-within? x] of t1 [ report false ]
      if not [y-within? y] of t2 [ report false ]
      report true
  ]
  ;; now handle the normal case where neither turtle is vertical;
  ;; start by representing lines in slope-intercept form (y=mx+c)
  let c1 [link-ycor - link-xcor * m1] of t1
  let c2 [link-ycor - link-xcor * m2] of t2
  ;; now solve for x
  let x (c2 - c1) / (m1 - m2)
  ;; check if intersection point lies on both segments
  if not [x-within? x] of t1 [ report false ]
  if not [x-within? x] of t2 [ report false ]
  report true
end

;; from the intersecting links example in Netlogo Model library by Uri Wilensky
to-report link-xcor
  report ([xcor] of end1 + [xcor] of end2) / 2
end

;; from the intersecting links example in Netlogo Model library by Uri Wilensky
to-report link-ycor
  report ([ycor] of end1 + [ycor] of end2) / 2
end

;; from the intersecting links example in Netlogo Model library by Uri Wilensky
to-report x-within? [x]  ;; turtle procedure
  report abs (link-xcor - x) <= abs (link-length / 2 * sin link-heading)
end

;; from the intersecting links example in Netlogo Model library by Uri Wilensky
to-report y-within? [y]  ;; turtle procedure
  report abs (link-ycor - y) <= abs (link-length / 2 * cos link-heading)
end

to-report get-p-value [here-x here-y other-x other-y]
;; p-values based upon actual local migration values, p52, Hagerstrand 1965
  let p-to-return 0
  let x-diff abs (here-x - other-x)
  let y-diff abs (here-y - other-y) 
  
  if x-diff = 0 [
      if y-diff = 0 [set p-to-return 0.4431] 
      if y-diff = 1 [set p-to-return 0.0547]
      if y-diff = 2 [set p-to-return 0.0168]    
  ]
  if x-diff = 1 [
      if y-diff = 0 [set p-to-return 0.0547] 
      if y-diff = 1 [set p-to-return 0.0301]
      if y-diff = 2 [set p-to-return 0.0140]    
  ]
  if x-diff = 2 [
      if y-diff = 0 [set p-to-return 0.0168] 
      if y-diff = 1 [set p-to-return 0.0140]
      if y-diff = 2 [set p-to-return 0.0096]    
  ]
  
  report p-to-return
end

to-report get-q-value [here-x here-y other-x other-y]
  ;;The equation for q is outlined on p56 Hagerstrand 1965
  ;; Note: really getting q for other x
  
  let q-to-return 0
  let p-valued get-p-value here-x here-y other-x other-y
  let other-patch one-of patches with [pxcor = other-x and pycor = other-y]
  let n-value count person-on other-patch
  set q-to-return (p-valued * n-value) / get-q-denominator here-x here-y
 
  report q-to-return

end

to-report get-q-denominator [here-x here-y]
  let q-denominator 0
  let x -2
  let y -2
   while [x < 3][
     while [y < 3] [
        let patch-calc one-of patches with [pxcor = here-x + x and pycor = here-y + y]
        if patch-calc != nobody [
        let p-valued get-p-value here-x here-y [pxcor] of patch-calc [pycor] of patch-calc
        set q-denominator q-denominator + (p-valued * (count person-on patch-calc))
        ]
       set y y + 1
     ]
     set x x + 1
     set y -2
   ]

  report q-denominator
end


to-report moore-offsets
  ;; since the space does not use negative numbers, and the radius of interaction does not change,
  ;; these values have been hardcoded
  let result [[-1 1] [2 -1] [-2 1] [-1 -1] [1 0] [-1 2] [2 0] [-2 0] [0 2] [-2 -1] [2 -2] [0 0] [2 1] [0 -1] [1 1] [0 -2] [1 -1] [1 -2] [-2 2] [0 1] [-2 -2] [-1 -2] [2 2] [-1 0] [1 2]]
     report result 

end



to add-1929-adopters  

;; Based on p58 of Hagerstrand 1965 which uses this distribution of historical 
;; first adopters as the input data for the model

  add-initial-innovation-adopter 3 6
  add-initial-innovation-adopter 3 6
  add-initial-innovation-adopter 3 7
  add-initial-innovation-adopter 3 7 
  add-initial-innovation-adopter 3 8
  add-initial-innovation-adopter 3 8
  add-initial-innovation-adopter 3 8
  add-initial-innovation-adopter 3 8
  add-initial-innovation-adopter 3 8
  add-initial-innovation-adopter 5 5
  add-initial-innovation-adopter 5 5
  add-initial-innovation-adopter 5 5
  add-initial-innovation-adopter 1 4
  add-initial-innovation-adopter 2 5
  add-initial-innovation-adopter 2 2
  add-initial-innovation-adopter 3 3
  add-initial-innovation-adopter 3 4
  add-initial-innovation-adopter 4 4
  add-initial-innovation-adopter 11 8
  add-initial-innovation-adopter 6 10
  add-initial-innovation-adopter 5 10
  add-initial-innovation-adopter 9 9  
end

to add-initial-innovation-adopter [xcord ycord]
  let start-patch patches with [pxcor = xcord and pycor = ycord]
  let adopters person-on start-patch
  let adopter one-of adopters with [innovation-adopted? = false]
  ifelse adopter !=  nobody [ask adopter [set innovation-adopted? true]]
  [
    ask one-of start-patch [sprout-person 1 [set color blue set size 0.25 set innovation-adopted? true]] 
  ]
  
end

to create-sweeden-barriers
  ;; the barriers are based upon landscape features in the area of Hagerstand's study.
  ;; Based on p58 figure 4 of Hagerstrand 1965 which shows this barrier configuration
  draw-barriers 0.5 6.5 0.5 9.5 true
  draw-barriers 4.5 11.49 4.5 10.5 true
  draw-barriers 2.5 6.5 2.5 5.5 true
  draw-barriers 6.5 11.49 6.5 9.5 true 
  draw-barriers 9.5 11.49 9.5 10.5 true
  draw-barriers 7.5 4.5 7.5 3.5 true
  draw-barriers 8.5 9.5 8.5 8.5 true
  draw-barriers 6.5 5.5 6.5 4.5 true
  draw-barriers 3.5 6.5 5.5 6.5 true
  draw-barriers 5.5 8.5 6.5 8.5 true
  draw-barriers 6.5 4.5 7.5 4.5 true
  draw-barriers 7.5 3.5 8.5 3.5 true
  draw-barriers 4.5 7.5 5.5 7.5 true 
  draw-barriers 2.5 8.5 2.5 7.5 false 
  draw-barriers 3.5 6.5 3.5 5.5 false
  draw-barriers 6.5 9.5 6.5 8.5 false
  draw-barriers 7.5 9.5 7.5 8.5 false 
  draw-barriers 1.5 1.5 1.5 0.5 false
  draw-barriers 5.5 1.5 5.5 0.5 false
  draw-barriers 11.5 2.5 11.5 3.5 false
  draw-barriers 12.5 5.5 12.5 4.5 false
  draw-barriers 11.5 9.5 11.5 7.5 false
  draw-barriers 4.5 2.5 6.5 2.5 false
  draw-barriers 2.5 6.5 3.5 6.5 false
  draw-barriers 10.5 7.5 12.5 7.5 false
  draw-barriers 6.5 8.5 7.5 8.5 false
  draw-barriers 4.5 7.5 4.5 6.5 false
  draw-barriers 5.5 3.5 6.5 3.5 false
  draw-barriers 9.5 2.5 12.5 2.5 false
  draw-barriers 7.5 2.5 7.5 1.5 true
  draw-barriers 6.5 1.5 7.5 1.5 false
  draw-barriers 12.5 7.5 12.5 6.5 false
  
end

to draw-barriers [xcor1 ycor1 xcor2 ycor2 half-link?]
   create-barrier-endpoints 1 [setxy xcor1 ycor1 set size 0.15 set color black set link-ref 1]
   create-barrier-endpoints 1 [setxy xcor2 ycor2 set size 0.15 set color black set link-ref 2]
   let barrier-to-link1 one-of barrier-endpoints with [link-ref = 1]
   let barrier-to-link2 one-of barrier-endpoints with [link-ref = 2]
   ifelse half-link?[ask barrier-to-link1 [create-half-link-with barrier-to-link2 [set color gray set thickness 0.15]] ]
   [ask barrier-to-link1 [create-barrier-link-with barrier-to-link2 [set color black set thickness 0.15]]]
   ask barrier-to-link1 [set link-ref  0]
   ask barrier-to-link2 [set link-ref  0]
end

to format-sweeden-area  
  let no-patch patches with [pxcor < 4 and pycor = 11]
  ask no-patch [set farm-here? false set pcolor black]
  set no-patch patches with [pxcor = 0 and pycor < 4]
  ask no-patch [set farm-here? false set pcolor black]
  set no-patch patches with [pxcor > 11 and pycor = 11]
  ask no-patch [set farm-here? false set pcolor black]
  set no-patch patches with [pxcor = 13 and pycor > 6]
  ask no-patch [set farm-here? false set pcolor black]
  set no-patch patches with [pxcor > 5 and pycor = 0]
  ask no-patch [set farm-here? false set pcolor black]
  set no-patch patches with [pxcor > 6 and pycor = 1]
  ask no-patch [set farm-here? false set pcolor black]
  set no-patch patches with [pxcor > 7 and pxcor < 11 and pycor = 2]
  ask no-patch [set farm-here? false set pcolor black]
  set no-patch patches with [pxcor > 11 and pycor = 2]
  ask no-patch [set farm-here? false set pcolor black]
  set no-patch patches with [pxcor = 13 and pycor = 3]
  ask no-patch [set farm-here? false set pcolor black]
end
@#$#@#$#@
GRAPHICS-WINDOW
352
60
922
571
-1
-1
40.0
1
10
1
1
1
0
0
0
1
0
13
0
11
1
1
1
ticks
30.0

BUTTON
16
38
101
71
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
106
38
186
71
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
191
38
273
71
go-once
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

CHOOSER
16
115
200
160
Model-Type
Model-Type
"Simple Isotropic Model" "Anisotropic Model"
1

SLIDER
17
695
201
728
max-farm-population
max-farm-population
0
50
31
1
1
NIL
HORIZONTAL

TEXTBOX
34
600
268
618
Anisotropic Model Variables
16
0.0
1

TEXTBOX
20
635
278
715
Since we are unable to find Hagerstrand's data on population per patch, the population is randomly distributed
13
0.0
1

TEXTBOX
12
746
358
794
It is assumed that each farm patch has at least 5 agents.
13
0.0
1

PLOT
14
189
310
389
Innovation Adopted
Time
Count
0.0
25.0
0.0
4000.0
true
true
"" ""
PENS
"default" 1.0 0 -5298144 true "" "plot total-adopted"

SWITCH
421
694
605
727
read-data-from-file
read-data-from-file
1
1
-1000

TEXTBOX
424
743
864
761
If inputing data from a file please input in (x, y, pop) format.
13
0.0
1

@#$#@#$#@
## WHAT IS IT?

The model is one of the earliest known calibrated and validated simulations with implicit "agent based" methodology. 

The purpose of this model is to understand the processes which create a “nebula-distribution,” a common spatial pattern. This model explores the way that the diffusion of ideas across a social network might produce this pattern. The model attempts to recreate the chronologic and geographic patterns produced in a historical example, the diffusion of the adoption of government subsidies for improved pasture on farms in Sweden. Two related models were outlined and studied by the author. The first model, referred to as the isotropic model, begins with a uniform population number and a 5x5 grid. This model is designed to show how innovation might spread on a limited scale. The second model, the anisotropic model, initializes with different numbers of agents on each patch, a larger more complex spatial area and is intended to recreate a specific case study. The models rely on real-world data to create some of the models’ parameters (e.g. probability distributions used in the model are calculated from actual migration data and telephone call data) and attempt to compare real world distributions of the spread of ideas with those predicted by the model.

## HOW IT WORKS

Isotropic: This model is constructed on a 5 x 5 square grid with 30 agents on each square. One agent in the center of the model is initialized with an idea that will be spread to the rest of the agents. The probability of an agent spreading an idea is based upon the receiving agents’ relative position to the agent spreading the innovation. 

Anisotropic: The model represents the study region with a series of squares designed to mimic the geography of the region. Geographic boundaries such as lakes or rivers are represented as boundaries over which ideas cannot pass, or the probability of an idea spreading is reduced by half. The model uses a heterogeneous population of agents on each cell based upon actual population and begins with several initial adopters based upon historical data of government subsidy adoption.

At each time step each agent that has adopted an innovation looks to its neighbors to try and spread the innovation. The agent can spread to other agents on the cell that it occupies, or it can spread to agents 2 patches away in a Moore neighborhood. Each agent must spread to an agent other than themselves. Agents that have received an innovation to adopt can still have an innovation spread to them. The model continues until all agents have adopted the innovation.


## CREDITS AND REFERENCES

This model is a replication of the model created by Torsten Hägerstrand in 1965.

Hägerstrand, Torsten (1965) ‘A Monte Carlo Approach to Diffusion’, Archives Européennes de Sociologie, 6(1), pp. 43-67.

It was replicated by Sean Bergin (sean.m.bergin@gmail.com) in 2012. The model was chosen for replication as a result of a contest or vote at the CoMSES network (www.openabm.org). This work and the CoMSES network is funded by a NSF grant (AWARD # 0909394) entitled: A Research Network for Computational Modeling in the Socioecological Sciences.

The model can be found at www.openabm.org along with a more detailed desctiption of the concepts and equations use. 
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
