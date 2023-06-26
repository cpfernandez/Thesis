/**
* Name: Prison Covid Model
* Author: Chien Carisse P. Fernandez
* Tags: SEIRD Model, ABM, COVID-19 Transmission,
*/
model Model
global {
	int init_pop;
	file hallways_shapefile <- file("../includes/sample-hallway.shp");
	file cells_shapefile <- file("../includes/sample-prison-cells.shp");	
	int total_pop <- init_pop update: init_pop - removed;
	float positivity_rate <- 0.001;
	float wandering_rate <- 0.10;
	//int init_infected <- round(init_pop * positivity_rate);
	int init_infected <- 10;
	int wandering_inmate <- round(init_pop * wandering_rate);
	int maxiter <- 365;
	float lambda <- 0.0;
	
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
			if (can_wander_outside = false){
				speed <- rnd(0.0,1.0);
				do wander bounds:assigned_prison_cell;
			} else {
				speed <- rnd(0.0,5.0);
				do wander;
			}
		} // end wander
		
		/* Function for probability of infection */	
		float inf(float n){
			float inf <- (((-18.19 * ln(n)) + 43.276) / 100) * (1 - lambda);
			write ("value at fcn " + inf);
			return inf;
			
		} // end inf
		
		reflex get_infected when:(is_exposed and incubation_duration = incubation_period){
			float r <- rnd(0.000,1.000);
			write("distance n: "+ distance); // checking
			write("random value r: "+ r); // checking
			if(r < inf(distance)){
				is_exposed <- false;
				is_infected <- true;
				infection_period <- rnd(13,18);
				write("INFECTED"); // checking
			}else{
				is_exposed <- false;
				is_susceptible <- true;
				write("NOT INFECTED"); // checking
			}
			distance <- 0.0;
			incubation_duration <- 0;
		} // end get_infected
		
		reflex count_incubation_period when: is_exposed{
			incubation_duration <- incubation_duration + 1;
		} // end count_incubation_period
		
		reflex get_exposed when: is_infected{
			ask inmate at_distance 2 #m{
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
	
	
	
	/* CALCULATE THE HIGHEST INFECTION RATE */
	float infection_rate update:infected/init_pop;
	float highest_infection_rate <- 0.0;
	reflex infection_rate when: infection_rate > highest_infection_rate{
		highest_infection_rate <- infection_rate;
	}
	
	/* CALCULATE THE HIGHEST DEATH RATE */
	float death_rate update:removed/init_pop;
	float highest_death_rate <- 0.0;
	reflex death_rate when: death_rate > highest_death_rate{
		highest_death_rate <- death_rate;
	}
	
	/* ENDING CONDITION */
	reflex end_simulation when: cycle_number = maxiter{
		do pause;
	}
	
}	
/************* EXPERIMENTS ************/
experiment map_only type:gui{
			
	output{
		display map refresh: every (1 #cycle){
			species hallway aspect: geom;
			species prison_cell aspect: geom;
			species inmate aspect: appearance;
		}
		monitor "Initial Population" value: init_pop;
		monitor "Current Population" value: total_pop;
		monitor "Number of infected agents" value:infected;
		monitor "Number of susceptible agents" value:susceptible;
		monitor "Number of recovered agents" value:recovered;
		monitor "Number of removed agents" value:removed;
		monitor "Highest infection rate" value: highest_infection_rate;
		monitor "Death Rate Peak" value: highest_death_rate;
		monitor "Wandering Inmate" value: wandering_inmate;
		
		display chart_display type: java2D refresh: every(5 #cycles) {
			chart "Evolution of Contagion" type: series {
				//data "susceptible" value: susceptible color: #blue;
				data "infected" value: infected color: #red;
				data "recovered" value: recovered color: #green;
				data "removed" value: removed color: #black;
				data "exposed" value: exposed color: #orange;
			}
		}
//		display outbreak_point refresh: every(5 #cycles){
//			chart "Infection Rate" type: series{
//				data "Infection rate" value: infection_rate;
//			}
//		}
		
		
		
		
	}
	
}

