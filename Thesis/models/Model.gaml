/**
* AN ANALYSIS OF THE EFFECT OF HIGH CONGESTION RATES 
* IN PHILIPPINE PRISONS ON SARS-CoV-2 PROPAGATION USING 
* SEIR AGENT-BASED MODELING APPROACH
* Author: Chien Carisse P. Fernandez
* Tags: SEIRD Model, ABM, COVID-19 Transmission
*/
model Model
global {
	int init_pop <- 679;
	file hallways_shapefile <- file("../includes/Leyte-hallway.shp");
	file cells_shapefile <- file("../includes/Leyte-prison-cells.shp");	
	int total_pop <- init_pop update: init_pop - removed;
	float wandering_rate <- 1.0;
	float using_face_mask_rate <- 0.0;
	int using_face_mask_inmate <- round(init_pop * using_face_mask_rate);
	float positivity_rate <- 0.01;
	int init_infected <- round(init_pop * positivity_rate);
	//int init_infected <- 10;
	int wandering_inmate <- round(init_pop * wandering_rate);
	int maxiter <- 365;
	float lambda <- 0.47;
	geometry shape <- envelope(hallways_shapefile);	
	
/************** INITAL STATE OF THE SIMULATION*******************/
	int infected <- init_infected update: inmate count (each.is_infected);
	int susceptible <- init_pop - init_infected update: inmate count (each.is_susceptible);
	int exposed <- 0 update: inmate count (each.is_exposed);
	int recovered <- 0 update: inmate count (each.is_recovered);
	int removed <- 0;
	int cycle_number <-0 update: cycle_number + 1;	
	
	
	init{
		/* Create the simulation space from the shapefiles */
		create hallway from: hallways_shapefile;
		create prison_cell from: cells_shapefile;
		
		/* Create agents and assign location of each randomly */
		create inmate number: init_pop{
			assigned_prison_cell <- one_of(prison_cell);
			location <- any_location_in(assigned_prison_cell);
		}
		
		
		/* Assign infection state to infected agents */
		ask init_infected among inmate{
			is_infected <- true;
			is_susceptible <- false;
			is_recovered <- false;
			is_exposed <- false;
		}
		/* Assign the movement status of each agent */
		ask wandering_inmate among inmate{
			can_wander_outside <- true;
		}
		
		ask using_face_mask_inmate among inmate{
			using_face_mask <- true;
		}
	} // end init
	
	
	species hallway{
		aspect geom{
			draw shape color:#antiquewhite border: #black;
		}
	} // end hallway
	
	species prison_cell{
		aspect geom{
			draw shape color:#white border:#black;
		}
	} // end prison_cell
	
	/* ****************** BEHAVIORAL RULES ******************* */
	species inmate skills:[moving]{
		bool is_exposed <- false;
		bool is_infected <- false;
		bool is_recovered <- false;
		bool is_susceptible <- true;
		bool can_wander_outside <- false;
		bool is_wandering_outside <- false;
		bool using_face_mask <- false;
		int infection_duration <- 0;
		int infection_period;
		int recovered_duration <- 0;
		int recovered_period;
		int incubation_duration <- 0;
		int incubation_period;
		float speed <- rnd(0.0, 5.0);
		prison_cell assigned_prison_cell;
		float distance <- 0.0;
				
		reflex wander{
			if flip(1.0){ // can change the value of flip()
				if (can_wander_outside = false){
				
					speed <- rnd(0.0,1.0);
					do wander bounds:assigned_prison_cell;
				
			} else {
				
					speed <- rnd(0.0,1.0);
					do wander;
			}
			}
		}
		

		/* Function for probability of infection */	
		float inf(float n){
			float inf <- (((-18.19 * ln(n)) + 43.276) / 100);
			return inf;
			
		} // end inf
		
		reflex get_infected when:(is_exposed and incubation_duration = incubation_period){
			float r <- rnd(0.000,1.000);
			float fcn_value <- inf(distance);
			
			if(using_face_mask = true){
				fcn_value <- fcn_value - (fcn_value * lambda);
			}
			
			if(r < fcn_value){
				is_exposed <- false;
				is_infected <- true;
				infection_period <- rnd(7,18);
			}else{
				is_exposed <- false;
				is_susceptible <- true;
			}
			distance <- 0.0;
			incubation_duration <- 0;
		} // end get_infected
		
		reflex count_incubation_period when: is_exposed{
			incubation_duration <- incubation_duration + 1;
		} // end count_incubation_period
		
		reflex get_exposed when: is_infected{
			ask inmate at_distance 1.5 #m{
				if is_susceptible{
					is_exposed <- true;
					is_susceptible <- false;
					incubation_period <- 6;
					
					// measure the distance of the susceptible
					// agent from the infected one
					distance <- self distance_to myself;
				}
			}
		} // end get_exposed
		
		
		reflex count_infection_duration when: is_infected{
			infection_duration <- infection_duration + 1;
		} // end count_infection_duration
		
		reflex remove_or_recover when:is_infected {
			if (infection_duration = infection_period){
				if ( rnd (0.000,1.000) <= 0.0224){
					removed <- removed + 1;
					do die;
				
				} else {
					is_recovered <- true;
					is_infected <- false;
					recovered_period <- 100; //100 days of immunity
				}
				infection_duration <- 0;
			}
			
		} // end remove_or_recover
		
		reflex count_recovered_days when: is_recovered{
			recovered_duration <- recovered_duration + 1;
		} // end count_recovered	
		
		reflex get_susceptible when: is_recovered{
			if recovered_duration = recovered_period{
				is_recovered <- false;
				is_susceptible <- true;
				recovered_duration <- 0;
			}
		} // end get_susceptible
			
		
		aspect appearance{
			if is_susceptible{
				draw circle(0.4) color: #blue;
			}
			if is_exposed{
				draw circle(0.4) color: #orange;
			}
			if is_infected{
				draw circle(0.4) color: #red;
			}
			if is_recovered{
				draw circle(0.4) color: #green;
			}	
		}
		
	}//end inmate
	
	
	float susceptible_rate <- 1.0  update:susceptible/init_pop;
	float exposure_rate update:exposed/init_pop;
	/* CALCULATE THE HIGHEST RECOVERY RATE */
	int highest_recovery_rate_day <- 1;
	float recovery_rate update:recovered/init_pop;
	float highest_recovery_rate <- 0.0;
	reflex recovery_rate when: recovery_rate > highest_recovery_rate{
		highest_recovery_rate <- recovery_rate;
		highest_recovery_rate_day <- cycle_number;
	}
	
	
	/* CALCULATE THE HIGHEST INFECTION RATE */
	int highest_infection_rate_day <- 1;
	float infection_rate update:infected/init_pop;
	float highest_infection_rate <- 0.0;
	reflex infection_rate when: infection_rate > highest_infection_rate{
		highest_infection_rate <- infection_rate;
		highest_infection_rate_day <- cycle_number;
	}
	
	/* CALCULATE THE HIGHEST DEATH RATE */
	float death_rate update:removed/init_pop;
	float highest_death_rate <- 0.0;
	reflex death_rate when: death_rate > highest_death_rate{
		highest_death_rate <- death_rate;
	}
	
	/* CALCULATE AVG DIFFERENCE BETWEEN EXPOSURE RATE AND INFECTION RATE */
	float exposure_to_infection <- exposure_rate - infection_rate update: exposure_rate - infection_rate;
	float avg_exposure_to_infection <- 0.0;
	reflex calculate_mean{
		avg_exposure_to_infection <- (avg_exposure_to_infection + exposure_to_infection) / 2;
	}
	
	
	/* ENDING CONDITION */
	reflex end_simulation when: cycle_number = maxiter{
		do pause;
	}
	
}	
/************* EXPERIMENTS ************/
experiment map_only type:gui{

	parameter "Wandering rate" var: wandering_rate ;
	output{
		

		monitor "Initial Population" value: init_pop;
		monitor "Current Population" value: total_pop;
		monitor "Number of infected agents" value:infected;
		monitor "Number of susceptible agents" value:susceptible;
		monitor "Number of recovered agents" value:recovered;
		monitor "Number of removed agents" value:removed;
		monitor "Highest infection rate" value: highest_infection_rate;
		monitor "Highest infection rate day" value: highest_infection_rate_day;
		monitor "Highest recovery rate" value: highest_recovery_rate;
		monitor "Highest recovery rate day" value: highest_recovery_rate_day;
		monitor "Average exposure to infection" value: avg_exposure_to_infection;
		monitor "Exposure to infection" value:exposure_to_infection;
		
		

		/* CREATION OF GRAPH */
		display SEIR type: java2D refresh: every(5 #cycles) {
			chart "Evolution of Contagion" type: series 
			x_label: "Day"
			y_label: "Rate"
			x_tick_line_visible: false
			y_range: [0,0.35] // can be modified depending
							 // maximum rate
		
			{
				
				// data "susceptible" value: susceptible_rate color: #blue ;
				data "exposed" value: exposure_rate color: #orange;
				data "infected" value: infection_rate color: #red;
				//data "recovered" value: recovery_rate color: #green ;
				data "removed" value: death_rate color: #black ;
				
			}
		}
	}
}// END OF CODE

