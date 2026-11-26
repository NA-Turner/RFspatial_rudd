##assinging rec buffers 
#Wells et al.


##########
#hamilton harbour polgyon file
shorelinemap <- read_sf("01_data/04_shapefiles/HH_Poly_Mar2025/HH_WaterLinesToPoly_21Mar2025.shp")
shorelinemap <- st_transform(shorelinemap, crs = 4326)


HH_plot<-ggplot(shorelinemap) +
 geom_sf(fill = "lightblue", color = "black", size = 0.5) +
 theme_minimal() +
 theme(axis.text = element_text(size = 8))
#use glatos rpackage to summarize detections by num_fish and num_dets

HH_plot

###########
#lake ontario water levels for 2024 and 2025 were right in line with the historical average 
#~75msl for the summer period 


#setting the thermocline and isocline buffer radius based on 
#Wells et al. paper

#########selecting approximate buffer ranges for fall and summer 
##offshore and nearshore
#all the west end recs (offshore and nearshore assigned to chain B)

#supp data frome Wells et al - Hamilton Harbour detection range testing

#############################
wellsrecs<-read_excel("01_data/02_processed_files/Wellsetal_suppdata_detectionrange.xlsx")

group_A <- wellsrecs[wellsrecs$Assignment == "A", ]
group_B <- wellsrecs[wellsrecs$Assignment == "B", ]
group_C <- wellsrecs[wellsrecs$Assignment == "C", ]
group_D <- wellsrecs[wellsrecs$Assignment == "D", ]
group_E <- wellsrecs[wellsrecs$Assignment == "E", ]

#use group A for pelagic offshore + south shore walls
#use group B for west end and nearshore recs

detectionrange_means<-wellsrecs %>%
  group_by(Assignment, Season) %>%
  summarise(
    group_mean = mean(Mean),
    group_min = mean(Min),
    group_max = mean(Max),
    n_depths = n()
  )


#buffers are as follows, west end and nearshore following group B
#pulled from supp data
#fall range = 413
sum(389+408+366+440+446+395)/6 #407
#summer range = 300
#IDs 6,5,13,3, 28, 4)
sum(309+322+296+276+314+284)/6

#pelagic offshore and southshore 
#fall range = 429
sum(435+450+414+414+420)/5
#426.6
#summer range = 275
#recs 21,22, 12, 11, 10
sum(297+290+266+264+264)/5
