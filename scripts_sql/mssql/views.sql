/*
 * Depends: constant.sql
 */

-------------------------
--   WICST Grid Views  --
-------------------------

-- pivot Lookuptable to show 4 plots per row
--drop procedure make_treatment_lookup; -- if needed
CREATE PROCEDURE MAKE_TREATMENT_LOOKUP 
	@endyear INT = 2100,
  	@startyear INT = 1989
AS 
BEGIN
	/*
	 *  Long Version
	 * 
	 *  Recreation of the treatment rotation grid for the core WICST trial. Currently the implementatio is a little ugly
	 */
	-- A little ugly, but gets the job done
	CREATE VIEW wicst.LookupCoreTreatment AS
	/*
	 * Recreation of the treatment rotation grid for the core WICST trial.
	 */
	WITH maxphase AS (
	    SELECT 
	        system_id, 
	        MAX(phase) AS phases
	    FROM wicst.rotations
	    GROUP BY system_id
	), yearseq AS (
	    SELECT 
	        theyear = n
	    FROM (SELECT TOP (2100 - 1989 + 1) 
	                 ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) + 1988 AS n
	          FROM master.common.spt_values) AS years
	), offs AS (
		select plot_id, block, theyear, start_year, theyear - (start_year - 1) as offf, system_id, p.treatment_id from yearseq 
		cross join plots p
		left join treatments t on p.treatment_id  = t.treatment_id
	), phases as (
		select plot_id, block, theyear, treatment_id, offs.system_id, start_year, CAST(offf AS INT) % CAST(phases AS INT) as phase from offs
		left join maxphase on offs.system_id = maxphase.system_id
	), plants as (
		select plot_id, block, theyear, start_year, p.system_id, treatment_id, p.phase + 1 as phase, crop from phases p
		left join rotations r on p.system_id = r.system_id and p.phase + 1 = r.phase 
	)
	select theyear as year, ,
	case
	  when theyear < start_year then 'fc'
	  else crop
	end as realcrop
	from plants;
	
	/*
	 *  Wide Version
	 */

	DECLARE @cols NVARCHAR(MAX), @query NVARCHAR(MAX)
	SELECT @cols = STRING_AGG(quotename(theyear), ',')
	FROM (
		SELECT DISTINCT theyear from wicst.LookupCoreTreatment -- depends on previous View create to pivot wider
		WHERE theyear BETWEEN @startyear and @endyear
	) as years;

	-- must be dynamic query to make use of sequence between @startyear and @endyear
	set @query = '
		CREATE VIEW wicst.LookupCoreTreatmentWidest AS
		with treatment_wide as 
		(
			select plot_id, block, treatment_id, ' + @cols + '
			from (select plot_id, block, treatment_id, theyear, realcrop from wicst.LookupCoreTreatment) as sourcetable
			pivot
			(
				MAX(realcrop)
				for theyear in (' + @cols + ')
			) as pivottable
			where plot_id like ''A%''
		)
		select treatment_id, [1] as "Rep 1", [2] as "Rep 2", [3] as "Rep 3", [4] as "Rep 4",' + @cols + '
		from  (select plot_id, treatment_id, block,' + @cols + ' from treatment_wide) as steptable
		pivot
		(
			MAX(plot_id)
		    for block in ([1], [2], [3], [4])
		) as pivotted;';
	
	-- replace existing view
	DROP VIEW IF EXISTS wicst.LookupCoreTreatmentWidest;
	EXEC sp_executesql @query;
END;

-- create the wider view
EXEC MAKE_TREATMENT_LOOKUP 2100;

-------------------------
-- Silage Grid Views --
-------------------------

-- trt for silage view
--drop view wicst.LookupSilageTreatmentWidest

CREATE VIEW wicst.LookupSilageTreatmentWidest AS
/* 
 * Wider version of Silage Rotation Treatments
 */
with b as (
select * from wicst.plots
	where left(plot_id, 1 ) = 'A'),
a AS (
SELECT num as year, 
	   CAST((num - 2017) AS INT) % 4 + 7 as cs4_trt,
	   CAST((num - 2017 + 1) AS INT) % 3 + 11 as cs5_trt
	   from common.seq(2050, 2018, 1)),
c as (
SELECT pivotted.year, cs4_trt, cs5_trt, [1] as "CS4 Rep 1", [2] as "CS4 Rep 2", [3] as "CS4 Rep 3", [4] as "CS4 Rep 4" from a
left join b
on a.cs4_trt = b.treatment_id
PIVOT
(
	MAX(plot_id)
	for block in ([1],[2],[3],[4])
) as pivotted
)
SELECT pivot2.year, cs4_trt, cs5_trt, "CS4 Rep 1", "CS4 Rep 2", "CS4 Rep 3", "CS4 Rep 4", [1] as "CS5 Rep 1", [2] as "CS5 Rep 2", [3] as "CS5 Rep 3", [4] as "CS5 Rep 4" from c
left join b
on c.cs5_trt = b.treatment_id
PIVOT
(
	MAX(plot_id)
	for block in ([1],[2],[3],[4])
) as pivot2;


CREATE VIEW wicst.LookupSilageTreatment AS
/* 
 * Longer version of Silage Rotation Treatments
 */
SELECT year, sys_rep, plot_id from wicst.LookupSilageTreatmentWidest
UNPIVOT 
(
	plot_id for sys_rep in ("CS4 Rep 1", "CS4 Rep 2", "CS4 Rep 3", "CS4 Rep 4", 
							"CS5 Rep 1", "CS5 Rep 2", "CS5 Rep 3", "CS5 Rep 4")
) as unpivoted;



-----------------
-- Yield Views --
-----------------
--select * from wicst.harvesting_summary hs;

-- working to calculate total loss fraction and loss table
--TODO: will need to change these tables to be yielding ID's
CREATE VIEW wicst.harvesting_summary as
/* 
 * These harvest numbers account for direct loss and systematic loss
 */
WITH aggs AS (
	select 
		harvesting_id, 
		loss_category,
		SUM(loss_fraction) as sys_loss,
		CAST(loss_category AS NVARCHAR(MAX)) as set_name,
		1 as set_size
	from wicst.systematiclosses s
	group by harvesting_id, loss_category -- group by is not allows in recursive union step, so aggregate first
	),
	intersections as (
	-- recursively join for creating all intersection sets
	select * from aggs
	UNION ALL 
		SELECT
			a.harvesting_id, a.loss_category,
			a.sys_loss * i.sys_loss, -- assume independence between loss categories
			CAST(i.set_name + ' + ' + a.loss_category AS NVARCHAR(MAX)) as set_name, -- save set name for debugging
			i.set_size + 1 as set_size
		FROM aggs a
		INNER JOIN intersections i ON (a.harvesting_id = i.harvesting_id AND a.loss_category < i.loss_category)
	),
	signed as (
	   select harvesting_id, loss_category,
	   case when set_size % 2 = 1 then +sys_loss else -sys_loss END AS signed_sys_loss -- flip sign according to inclusion/exclusion formula
	   from intersections
	),
	combined_system_losses as (
	    /*
	     * Aggregate the system losses table
	     */
		select harvesting_id, sum(signed_sys_loss) as total_loss_fraction from signed
		group by harvesting_id
	),
	combined_direct_losses as (
		/*
		 * Aggregate the direct losses table
		 */
		select harvesting_id, sum(loss_area) as total_loss_area from wicst.directlosses
		group by harvesting_id
	)
	select wp.block, wp.treatment_id, wt.system_id, h.*, s.total_loss_fraction, d.total_loss_area, calc.*
	from wicst.harvestings h
	left join combined_system_losses as s on h.harvesting_id = s.harvesting_id
	left join combined_direct_losses AS d ON h.harvesting_id = d.harvesting_id
	left join wicst.plots as wp on h.plot_id = wp.plot_id
	left join wicst.treatments as wt on wp.treatment_id = wt.treatment_id
	cross apply (
	select
	    year                  = YEAR(harvest_date),
		adjusted_harvest_area = (harvest_area - coalesce(total_loss_area, 0)) * (1 - coalesce(total_loss_fraction, 0)), -- subtract direct area first, then probabilistic removed.
		dry_matter_lbs        = (100 - percent_moisture) / 100 * harvest_lbs,
		dry_matter_tons       = (100 - percent_moisture) / 100 * harvest_lbs / 2000,
		acre_frac             = (harvest_area / common.CONST_ACRE_TO_FT2()),
		lbs_per_acre          = harvest_lbs / (harvest_area / common.CONST_ACRE_TO_FT2()),
		ideal_moisture        = common.CONST_IDEAL_MOISTURE(product),
		moisture_correction         = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)),
		corrected_lbs               = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)) * harvest_lbs,
		corrected_lbs_per_acre      = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)) * harvest_lbs / (harvest_area / common.CONST_ACRE_TO_FT2()),
		corrected_lbs_per_adjusted_acre = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)) * harvest_lbs / ((harvest_area - coalesce(total_loss_area, 0)) * (1 - coalesce(total_loss_fraction, 0)) / common.CONST_ACRE_TO_FT2()),
		ideal_bushel                = common.CONST_IDEAL_BUSHEL(product),
		ideal_bu_per_acre           = harvest_lbs / (harvest_area / common.CONST_ACRE_TO_FT2()) / common.CONST_IDEAL_BUSHEL(product),
		ideal_bu_per_adjusted_acre  = harvest_lbs / ((harvest_area - coalesce(total_loss_area, 0)) * (1 - coalesce(total_loss_fraction, 0)) / common.CONST_ACRE_TO_FT2()) / common.CONST_IDEAL_BUSHEL(product),
		ideal_corrected_bu_per_acre = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)) * harvest_lbs / (harvest_area / common.CONST_ACRE_TO_FT2()) / common.CONST_IDEAL_BUSHEL(product),
		ideal_corrected_bu_per_adjusted_acre = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)) * harvest_lbs / ((harvest_area - coalesce(total_loss_area, 0)) * (1 - coalesce(total_loss_fraction, 0)) / common.CONST_ACRE_TO_FT2()) / common.CONST_IDEAL_BUSHEL(product),
		dry_matter_tons_per_acre             = (100 - percent_moisture) / 100 * harvest_lbs / 2000 / (harvest_area / common.CONST_ACRE_TO_FT2()),
		dry_matter_tons_per_adjusted_acre   = (100 - percent_moisture) / 100 * harvest_lbs / 2000 / ((harvest_area - coalesce(total_loss_area, 0)) * (1 - coalesce(total_loss_fraction, 0)) / common.CONST_ACRE_TO_FT2()),
		guess_percent_moisture              = common.GUESS_MOISTURE(year(harvest_date), product),
	    guess_dry_matter_lbs                = (100 - common.GUESS_MOISTURE(year(harvest_date), product)) / 100 * harvest_lbs,
		guess_dry_matter_tons               = (100 - common.GUESS_MOISTURE(year(harvest_date), product)) / 100 * harvest_lbs / 2000,
		guess_dry_matter_tons_per_acre            = (100 - common.GUESS_MOISTURE(year(harvest_date), product)) / 100 * harvest_lbs / 2000 / (harvest_area / common.CONST_ACRE_TO_FT2()),
		guess_dry_matter_tons_per_adjusted_acre   = (100 - common.GUESS_MOISTURE(year(harvest_date), product)) / 100 * harvest_lbs / 2000 / ((harvest_area - coalesce(total_loss_area, 0)) * (1 - coalesce(total_loss_fraction, 0)) / common.CONST_ACRE_TO_FT2())		
	) as calc

	
--------------- Pasture Yields View ----------------
----------------------------------------------------
CREATE VIEW wicst.pasture_yield_summary AS
with harvestpast AS (
	select hs.*, hd.tenday, hd.cycle
	from wicst.harvesting_summary hs
	left join wicst.harvestingdetails hd on hs.harvesting_id = hd.harvesting_id
	where product = 'pasture' and (harvest_lbs  > 0 or harvest_lbs IS NULL)
), biopast AS (
	select b.*, bd.cycle,
	--TODO: biomass yield, not accounting for loss in the pastures
	-- dry matter tons acre
	dmta = biomass_grams / 1000 * common.CONST_KG_TO_LBS() / 2000 * (100 - percent_moisture) / 100 / (biomass_area / common.CONST_ACRE_TO_FT2())
	from wicst.biomassings b
	left join wicst.biomassingdetails bd on b.biomassing_id = bd.biomassing_id
	where biomass = 'pasture' and year(biomass_date) between 1990 and 2023 --TODO remove year concstaints for for generalizability
), 
-- combine all biomassings and harvestings into single table for 
pastyield AS (
	SELECT *
	FROM (
		select plot_id, harvest_date as date, year(harvest_date) as year,
			dry_matter_tons_per_adjusted_acre as yield,
			method = 'harvest',
			cycle
		from harvestpast
		where year(harvest_date) between 1990 and 2023
		UNION ALL
		select 
			plot_id, 
			biomass_date as date, 
			year(biomass_date) as year,
		    dmta as yield,
		    method,
		    cycle
		from biopast
	) as cb
), pastyield_imputed AS (
	select *
	from (select plot_id, harvest_date as date, year(harvest_date) as year,
			coalesce(dry_matter_tons_per_adjusted_acre, guess_dry_matter_tons_per_adjusted_acre) as yield,
			method = 'harvest',
			cycle
		from harvestpast
		where year(harvest_date) between 1990 and 2023
		UNION ALL
		select plot_id, biomass_date as date, year(biomass_date) as year,
		    dmta as yield,
		    method,
		    cycle
		from biopast
	) as cb
), 
-- Three levels of sum/averaging/maxing for quadrat + harvesting
-- 1. avg/max subsample
-- 2. avg/max cycle
-- 3. sum plot
pastyield_lvl1 AS (
	select 
		date, year,	plot_id, cycle,
		avg_yield = AVG(yield),
		max_yield = MAX(yield)
	from pastyield
	where method IN ('quadrat', 'harvest')
	group by year, date, plot_id, cycle -- assumes quadratting on same day is each a subsample
), pastyield_lvl2 AS (
	select year, plot_id, cycle,
	avg_avg_yield = AVG(avg_yield),
	max_max_yield = MAX(max_yield)
	from pastyield_lvl1
	group by year, plot_id, cycle -- data points without cycle get averaged into NA category
), pastyield_lvl3 AS (
	select year, plot_id,
	sum_avg_avg_yield = SUM(avg_avg_yield),
	sum_max_max_yield = SUM(max_max_yield)
	from pastyield_lvl2
	group by year, plot_id
), 
-- Three levels of sum/averaging/maxing for just quadrats
-- 1. avg/max subsample
-- 2. avg/max cycle
-- 3. sum plot
pastyield_lvl1_noharvest AS (
	select 
		date, year,	plot_id, cycle,
		avg_yield = AVG(yield),
		max_yield = MAX(yield)
	from pastyield
	where method = 'quadrat'
	group by year, date, plot_id, cycle  -- assumes quadratting on same day is each a subsample
), pastyield_lvl2_noharvest AS (
	select year, plot_id, cycle,
	avg_avg_yield = AVG(avg_yield),
	max_max_yield = MAX(max_yield)
	from pastyield_lvl1_noharvest
	group by year, plot_id, cycle
), pastyield_lvl3_noharvest AS (
	select year, plot_id,
	sum_avg_avg_yield = SUM(avg_avg_yield),
	sum_max_max_yield = SUM(max_max_yield)
	from pastyield_lvl2_noharvest
	group by year, plot_id
), pastyield_lvl1_harvestexclosures AS (
	select 
		year, plot_id, -- exclosures do not have cycle info
		sum_yield = SUM(yield),
		count_harvestexclosures = count(yield)
	from pastyield
	where method IN ('harvest', 'exclosure')
	group by year, plot_id
), pastyield_lvl1_exclosures AS (
	select 
		year, plot_id, -- exclosures do not have cycle info
		sum_yield = SUM(yield),
		count_exclosures = count(yield)
	from pastyield
	where method IN ('exclosure')
	group by year, plot_id
), pastyield_imputed_lvl1_harvestexclosures AS (
	select
		year, plot_id,
		sum_yield = SUM(yield),
		count_harvestexclosures = count(yield)
	from pastyield_imputed
	where method IN ('harvest', 'exclosure')
	group by year, plot_id
), pastyield_imputed_lvl1_exclosures AS (
	select
		year, plot_id,
		sum_yield = SUM(yield),
		count_exclosures = count(yield)
	from pastyield_imputed
	where method IN ('exclosure')
	group by year, plot_id
), pastyield_imputed_lvl1_harvests AS (
	select
		year, plot_id,
		sum_yield = SUM(yield),
		count_harvests = count(yield)
	from pastyield_imputed
	where method IN ('harvest')
	group by year, plot_id
), pasture_year_plot AS (
	-- outer product of years and pasture plots
	select 
		num as year,	
		pasture_plots.plot_id
	from common.SEQ(2023, 1990, 1)
	cross join (
		select *
		from (VALUES ('A112'),('A207'),('A302'),('A405')) as ConstTable(plot_id)
	) as pasture_plots
), pasture_counts_long AS (
	select year, plot_id, method, count(*) as n
	from pastyield
	group by plot_id, year, method
), pasture_counts_wide AS (
	SELECT [year], [plot_id], ISNULL([harvest], 0) as 'n_harvest', ISNULL([quadrat], 0) as 'n_quadrat', ISNULL([exclosure], 0) as 'n_exclosure' from pasture_counts_long 
	PIVOT (
		max(n)
		FOR [method] in ([harvest], [quadrat], [exclosure])
	) as pt
)
select 
	pyp.*, pcw.n_harvest, 
	ih.sum_yield as sum_imputed_yield_harvests,
	pcw.n_quadrat, pcw.n_exclosure,
	ie.sum_yield as sum_imputed_yield_exclosures,
	h.sum_avg_avg_yield as sum_avg_avg_yield_harvestquadrat,
	0.75 * h.sum_avg_avg_yield as sum_avg_avg_yield_harvestquadrat_stubble3in,
	h.sum_max_max_yield as sum_max_max_yield_harvestquadrat,
	nh.sum_avg_avg_yield as sum_avg_avg_yield_quadrat,
	0.75 * nh.sum_avg_avg_yield as sum_avg_avg_yield_quadrat_stubble3in,
	nh.sum_max_max_yield as sum_max_max_yield_quadrat,
	he.sum_yield as sum_yield_harvestexclosures,
	e.sum_yield as sum_yield_exclosures,
	ihe.sum_yield as sum_imputed_yield_harvestexclosures
from pasture_year_plot pyp
left join pastyield_lvl3 h on pyp.year = h.year and pyp.plot_id = h.plot_id
left join pastyield_lvl3_noharvest nh on pyp.year = nh.year and pyp.plot_id = nh.plot_id
left join pastyield_lvl1_harvestexclosures he on pyp.year = he.year and pyp.plot_id = he.plot_id
left join pastyield_lvl1_exclosures e on pyp.year = e.year and pyp.plot_id = e.plot_id
left join pastyield_imputed_lvl1_harvestexclosures ihe on pyp.year = ihe.year and pyp.plot_id = ihe.plot_id
left join pastyield_imputed_lvl1_exclosures ie on pyp.year = ie.year and pyp.plot_id = ie.plot_id
left join pastyield_imputed_lvl1_harvests ih on pyp.year = ih.year and pyp.plot_id = ih.plot_id
left join pasture_counts_wide pcw on pyp.year = pcw.year and pyp.plot_id = pcw.plot_id
	
--select * from wicst.pasture_yield_summary order by year, plot_id

--drop view wicst.pasture_yield_summary
-- drop view wicst.harvesting_summary 
	
-- A version with imputed missing values
-- imputation table?
	
--SELECT *, 
--	imputed_percent_moisture = coalesce(percent_moisture, common.GUESS_MOISTURE(year(harvest_date), product))
--FROM wicst.harvestings
--WHERE year(harvest_date) = 2014;


--
--SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 'pasture_yield_summary';
--
--select * from INFORMATION_SCHEMA.COLUMNS
--	
	
-------------- Animal Grazing View -----------------
----------------------------------------------------
CREATE VIEW wicst.grazing_summary AS
SELECT
	*,
	num_days           = DATEDIFF(day, on_date, off_date),
	weight_gain        = (end_lbs - start_lbs),
	average_weight     = (end_lbs + start_lbs) / 2,
	average_daily_gain = (end_lbs - start_lbs) / DATEDIFF(day, on_date, off_date)
FROM wicst.grazings







	

